;;; A sack application HTTP server.
;;;
;;; Copyright (c) 2008-2009 Per Eckerdal, 2005-2007 Marc Feeley, All
;;; Rights Reserved.

;; TODO To specify to the server that the response is already
;; chunk-encoded, you have to specify a header like
;; ("Transfer-Encoding" . "chunked"), that is, the value must not
;; contain any whitespace, commas or any other values (for instance "
;; chunked" or "identity, chunked"). Otherwise it will not understand
;; it. To specify more than one encoding, use several header field
;; definitions.

;; TODO Implement pipelining

;; TODO Implement maximal chunk size on chunked encoding. [how to do that?]

;; TODO Implement conditional gets

;; TODO Implement partial gets

(import (only: (std srfi/13)
               string-downcase!
               string-downcase
               reverse-list->string)
        (only: (std srfi/19)
               current-date)
        (only: (std srfi/1)
               filter)
        (only: (std string/util)
               string-split)
        (only: uri
               uri-port-set
               uri-host-set
               uri-scheme-set
               uri-path
               uri-query
               parse-uri)
        (only: http-common
               date->rfc1123
               display-crlf
               display-header
               display-headers
               http-status-code
               permissive-read-line
               read-header)
        (std misc/token-table))

(declare
  (standard-bindings)
  (extended-bindings)
  (block))

(export sack-start!)

;==============================================================================

; Utilities.

;; The parameter name is assumed to be lowercase.
(define (has-header? headers name)
  (let loop ((lst headers))
    (cond
     ((null? lst) #f)
     ((equal? name
              (string-downcase (caar lst))) #t)
     (else (loop (cdr lst))))))

;; The parameter name is assumed to be lowercase. Note that this
;; function does not understand comma-separated lists of in header
;; values.
(define (has-header-with-value? headers name value)
  (let loop ((lst headers))
    (cond
     ((null? lst)
      #f)
     
     ((and (equal? name
                   (string-downcase (caar lst)))
           (equal? value
                   (cdar lst)))
      #t)
     
     (else
      (loop (cdr lst))))))

;==============================================================================

; HTTP server.

(define (sack-start! sack-application
                     #!key
                     (port-number 80)
                     (threaded? #t)
                     (keep-alive-timeout 15)
                     (keep-alive 5)
                     (timeout 300))
  (let ((server-port
         (open-tcp-server
          (list port-number: port-number
                backlog: 128
                reuse-address: #t
                server-address: "*"
                ;; server-address: '#u8(127 0 0 1) ; on localhost interface only
                char-encoding: 'UTF-8)))
        (serve
         (lambda (connection)
           (serve-connection sack-application
                             keep-alive-timeout
                             timeout
                             connection
                             threaded?
                             port-number
                             keep-alive)))

        ;; A mutex that is unlocked when the server should quit.
        (quit-mutex (make-mutex)))
    (mutex-lock! quit-mutex)
    
    (thread-start!
     (make-thread
      (lambda ()
        (let loop ()
          (let ((connection (read server-port)))
            (if threaded?
                ;; Multithreaded mode.
                (let ((dummy-port (open-dummy)))
                  (parameterize
                   ((current-input-port dummy-port)
                    (current-output-port dummy-port))
                   (thread-start!
                    (make-thread
                     (lambda ()
                       (serve connection))))))
                
                ;; Single-threaded mode.
                (serve connection)))
          
          ;; If the mutex is not locked, it means that we should quit.
          (if (not (mutex-lock! quit-mutex 0))
              (loop))))))

    (lambda ()
      (if (mutex-lock! quit-mutex 0)
          (error "Server has already quit (or is quitting)")
          (begin
            (mutex-unlock! quit-mutex)
            (close-port server-port))))))

;==============================================================================

; Error functions.

(define (show-error code str connection)
  (display-crlf connection "HTTP/1.1 " code " " (http-status-code code))
  (display-header connection
                  `("Content-Length" . ,(string-length str)))
  (display-crlf connection)
  (display str connection)
  (close-port connection))

(define (method-not-implemented-error connection)
  (show-error
   501
   "<html><head><title>501 Method Not Implemented</title></head>\
    <body><h1>Method Not Implemented</h1></body></html>"
   connection))

(define (internal-server-error connection)
  (show-error
   500
   "<html><head><title>500 Internal Server Error</title></head>\
    <body><h1>Internal Server Error</h1></body></html>"
   connection))

(define (bad-request-error connection)
  (show-error
   400
   "<html><head><title>400 Bad Request</title></head>\
    <body><h1>Bad Request</h1></body></html>"
   connection))

;------------------------------------------------------------------------------

; Sack functions.

(define (make-environment threaded?
                          uri
                          request-method
                          ;; Attributes is an alist of lowercase
                          ;; header names and their values
                          attributes)
  (let ((headers
         (lambda (name)
           (if (procedure? name)
               (for-each (lambda (pair)
                           (name (car pair) (cdr pair)))
                         attributes)
               (map cdr
                    (filter (lambda (pair)
                              (equal? (car pair) name))
                            attributes))))))
    (lambda (name)
      (case name
        ((sack:version) '(0 2))
        ((sack:request-method) request-method)
        ((sack:headers) headers)
        ((sack:root) "")
        ((sack:uri) uri)
        ((sack:single-thread?) (not threaded?))
        
        (else #f)))))

(define hex "0123456789abcdef")

(define (hex-formatter num)
  (let ((gssymlen (string-length hex)))
    (reverse-list->string
     (let loop ((num num))
       (let ((idx (modulo num gssymlen)))
         (cons (string-ref hex idx)
               (if (eq? idx num)
                   '()
                   (loop (/ (- num idx) gssymlen)))))))))

(define (handle-sack-response-headers conn
                                      version
                                      code
                                      headers
                                      chunked-encoding?
                                      close-connection?
                                      has-close-header?)
  ;; Display headers
  (display-crlf conn version " " code " " (http-status-code code))
  
  (if (not (has-header? headers "date"))
      (display-header conn `("Date" . ,(date->rfc1123
                                        (current-date 0)))))
  
  (if (and close-connection?
           (not has-close-header?))
      (display-header conn `("Connection" . "close")))
  
  (display-headers conn headers)

  ;; It's important that this header is sent after the other headers,
  ;; because they might contain other transfer encodings, and chunked
  ;; should always be the last one.
  (if chunked-encoding?
      (display-header conn `("Transfer-Encoding" . "chunked")))

  (display-crlf conn))

(define (handle-sack-response-write-chunked conn response-thunk)
  (let loop ()
    (let* ((message (response-thunk))
           (len (if message
                    (u8vector-length message)
                    0)))
      ;; We must not send a chunk of zero length,
      ;; since that means the end of the response.
      (if (not (zero? len))
          (begin
            (display-crlf conn (hex-formatter len))
            (write-subu8vector
             message
             0
             (u8vector-length message)
             conn)
            (display-crlf conn)
            (force-output conn)))
      (if message (loop))))
  
  ;; Send the last chunk
  (display-crlf conn "0")
  (display-crlf conn))

;; Returns #t if the connection was NOT closed after this response
(define (handle-sack-response keep-alive
                              sack-application
                              environment
                              conn
                              version)
  (with-input-from-port conn
    (lambda ()
      (call-with-values
          (lambda () (sack-application environment))
        (lambda (code headers response-thunk)
          (let* ((respond
                  (lambda ()
                    (let loop ()
                      (let ((response
                             (response-thunk)))
                        (cond
                         ((not response))
                         
                         ((u8vector? response)
                          (write-subu8vector response
                                             0
                                             (u8vector-length response)
                                             conn)
                          (loop))

                         (else
                          (error "Invalid response thunk return value"
                                 response)))))))
                 
                 (chunked-encoding?
                  (and (not (has-header? headers "content-length"))
                       (not (has-header-with-value? headers
                                                    "transfer-encoding"
                                                    "chunked"))))
                 
                 (has-close-header?
                  (has-header-with-value? headers
                                          "connection"
                                          "close"))
                 
                 (close-connection?
                  (or (not (eq? 'HTTP/1.1 version))
                      (member "close"
                              ((environment 'sack:headers) "connection"))
                      has-close-header?
                      (<= keep-alive 1))))

            (case version
              ((HTTP/1.1 HTTP/1.0)
               ;; Display response headers
               (handle-sack-response-headers conn
                                             version
                                             code
                                             headers
                                             chunked-encoding?
                                             close-connection?
                                             has-close-header?)
               
               ;; Display response body
               (if (not (equal? "head"
                                (environment 'sack:request-method)))
                   (if chunked-encoding?
                       (handle-sack-response-write-chunked conn response-thunk)
                       (with-output-to-port conn respond))))
              
              (else
               ;; Unknown HTTP version. Just dump the body.
               (with-output-to-port conn respond)))
            
            (if close-connection?
                (close-port conn)
                (force-output conn))
            
            (not close-connection?)))))))

;------------------------------------------------------------------------------

; Low-level serving functions.

(define version-table
  (make-token-table
   ("HTTP/1.0" 'HTTP/1.0)
   ("HTTP/1.1" 'HTTP/1.1)))

(define (set-timeout! connection timeout)
  ;; Configure the connection with the client so that if we can't read
  ;; the request after [timeout] seconds, the read/write operation
  ;; will fail (and the thread will terminate).
  (input-port-timeout-set! connection
                           timeout
                           (lambda ()
                             (close-port connection)
                             #f))
  (output-port-timeout-set! connection
                            timeout
                            (lambda ()
                              (close-port connection)
                              #f)))

(define (serve-connection sack-application
                          keep-alive-timeout
                          timeout
                          connection
                          threaded?
                          port-number
                          keep-alive)
  (set-timeout! connection keep-alive-timeout)
  
  (let ((req (permissive-read-line connection)))
    (if (not (string? req))
        (bad-request-error connection)
        (begin
          (set-timeout! connection timeout)
          (let* ((end
                  (let loop ((i 0))
                    (cond ((= i (string-length req))
                           #f)
                          ((char=? (string-ref req i) #\space)
                           i)
                          (else
                           (loop (+ i 1))))))
                 (method-name
                  (let ((m (and end (substring req 0 end))))
                    (string-downcase! m)
                    m)))
            
            ;; Returns #t if the connection was kept alive
            (define (handle-version version uri)
              (case version
                ((HTTP/1.0 HTTP/1.1)
                 (let ((attributes (read-header connection)))
                   (cond
                    ((or (not attributes))
                     (bad-request-error connection)
                     #f)
                    
                    ((and (eq? 'HTTP/1.1 version)
                          (not (has-header? attributes "host")))
                     (bad-request-error connection)
                     #f)
                    
                    (else
                     (handle-request version attributes uri)))))
                ((#f)
                 ;; this is an HTTP/0.9 request
                 (handle-request 'HTTP/0.9 '() uri))
                (else
                 (bad-request-error connection)
                 #f)))
            
            ;; Returns #t if the connection was kept alive
            (define (handle-request version attributes uri)
              ;; Invoke handler (i.e. page generator)
              (with-exception-catcher
               (lambda (e)
                 (pp e) ;; TODO
                 (internal-server-error connection)
                 #f)
               (lambda ()
                 ;; Add some more info to the uri object. This is
                 ;; useful for the sack environment object.
                 (let* ((host/port (let ((ret (assoc "host" attributes)))
                                     (and ret (cdr ret))))
                        (host+port (string-split #\: (or host/port
                                                         "0.0.0.0"))))
                   (set! uri
                         (uri-port-set uri
                                       (or (and (pair? (cdr host+port))
                                                (string->number
                                                 (cadr host+port)))
                                           port-number)))
                   (set! uri (uri-host-set uri (car host+port)))
                   (set! uri (uri-scheme-set uri "http")))
                 
                 (handle-sack-response
                  keep-alive
                  sack-application
                  (make-environment threaded?
                                    uri
                                    method-name
                                    attributes)
                  connection
                  version))))
            
            (if method-name
                (parse-uri
                 req
                 (+ end 1)
                 (string-length req)
                 #t
                 (lambda (uri i)
                   (if (cond
                        ;; Return #t if the connection was kept alive
                        ((not uri)
                         (bad-request-error connection)
                         #f)
                        
                        ((not (< i (string-length req)))
                         (handle-version #f uri))
                        
                        ((not (char=? (string-ref req i) #\space))
                         (bad-request-error connection)
                         #f)
                        
                        (else
                         (let ((version-index
                                (token-table-lookup-substring
                                 version-table
                                 req
                                 (+ i 1)
                                 (string-length req))))
                           (if version-index
                               (handle-version
                                (vector-ref version-table
                                            (+ version-index 1))
                                uri)
                               (begin
                                 (bad-request-error connection)
                                 #f)))))
                       (serve-connection sack-application
                                         keep-alive-timeout
                                         timeout
                                         connection
                                         threaded?
                                         port-number
                                         (- keep-alive 1)))))
                
                (method-not-implemented-error connection)))))))

;==============================================================================
