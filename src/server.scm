;;; A sack application HTTP server.
;;;
;;; Copyright (c) 2008-2009 Per Eckerdal, 2010-2012 Mikael More, 2005-2007 Marc Feeley, All
;;; Rights Reserved.
;;; MIT license.

;; History
;; 2011-02-01: Added level to selected force-output calls to ensure that data is actually
;;             transmitted to the client immediately on chunk write.
;; 2012-02-18: Updated the calling convention of sack:body to a more effective and functional one.
;;             See the comments inside handle-sack-response for more info.

;; TODO To specify to the server that the response is already
;; chunk-encoded, you have to specify a header like
;; ("Transfer-Encoding" . "chunked"), that is, the value must not
;; contain any whitespace, commas or any other values (for instance
;; "chunked" or "identity, chunked"). Otherwise it will not understand
;; it. To specify more than one encoding, use several header field
;; definitions.

;; TODO Implement pipelining

;; TODO Implement maximal chunk size on chunked encoding. [how to do that?]

;; TODO Implement conditional gets

;; TODO Implement partial gets

;; TODO Test that reception of chunked encoding content really works. 

(import (only: (std srfi/13)
               string-downcase!
               string-downcase
               reverse-list->string)
        (only: (std srfi/19)
               current-date)
        (only: (std srfi/1)
               filter)
        (only: (std string/util)
               string-split-char)
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
        (std misc/token-table misc/exception))

(declare
  (standard-bindings)
  (extended-bindings)
  (block))

(export sack-start!)

;==============================================================================

; Utilities.

;; For making debug output
(define console-output-port (current-output-port))

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
                     (max-waiting-connections #f)
                     (threaded? #t)
                     (keep-alive-timeout 15)
                     (keep-alive 5)
                     (timeout 300)
                     (io-error-callback (lambda (e error-specific-to-a-connection?) #f))
                     (thread-name 'auto))
  (let ((server-port
         (open-tcp-server
          `(port-number: ,port-number
            ,@(if max-waiting-connections `(backlog: ,max-waiting-connections) '())
            reuse-address: #t
            server-address: "*"
            ;; server-address: '#u8(127 0 0 1) ; on localhost interface only
            char-encoding: UTF-8)))
        (serve
         (lambda (connection)
           (with-exception/continuation-catcher
            (lambda (e)
              (io-error-callback e #t)
              (list 'terminated-by-exception e)) ; Leaves thread termination message
            (lambda ()
              (serve-connection sack-application
                                keep-alive-timeout
                                timeout
                                connection
                                threaded?
                                port-number
                                keep-alive)
              'terminated-normally))))

        ;; A mutex that is unlocked when the server should quit.
        (quit-mutex (make-mutex)))
    (mutex-lock! quit-mutex)
    
    (thread-start!
     (let ((thunk (lambda ()
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
                                   (serve connection))
                                 '(http-server connection-handler-thread)))))

                            ;; Single-threaded mode.
                            (serve connection)))

                      ;; If the mutex is not locked, it means that we should quit.
                      (if (not (mutex-lock! quit-mutex 0))
                          (loop)))))
           (thread-name (if (eq? thread-name 'auto)
                            `(http-server connection-listener-thread port: ,port-number)
                            thread-name)))
       (if thread-name
           (make-thread thunk thread-name)
           (make-thread thunk))))

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
  (display-header connection `("Content-Length" . ,(string-length str)))
  (display-header connection `("Content-Type" . "text/html; char-encoding=UTF-8"))
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
                            attributes)))))
        (sack:body #f))
    (lambda (name)
      (case name
        ((sack:version) '(0 2))
        ((sack:request-method) request-method)
        ((sack:headers) headers)
        ((sack:root) "")
        ((sack:uri) uri)
        ((sack:single-thread?) (not threaded?))
        ((sack:body) ; The first call to sack:body is made by the internal handle-sack-response procedure.
                     ; It sets the sack:body variable to the right handler procedure, which is subsequently
                     ; returned on each call to 'sack:body .
                     (or sack:body
                         (lambda (sack:body-to-set)
                           (set! sack:body sack:body-to-set))))

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

  (display-crlf conn)
  (force-output conn))

; This procedure reads the incoming HTTP request body. The reading work is done in units of the
; read buffer or the HTTP chunk size, whichever is smallest. The mechanism is structured in such a
; way that it can be invoked over and over. Here's the convention:
;
; (make-sack-request-body-handler environment connection)
; => ret
;
; ret = #f = nothing was read and there's nothing more to read
;       reader-thunk w args (data-thunk copying?), that when invoked, returns another ret.
;       (box reader-thunk) = same as the previos option, except, the data-thunk returned #f.
; Note that reader-thunk :s are single-use.
(define (make-sack-request-body-handler environment connection)
  ; Read incoming HTTP request body
  ; (with-output-to-port console-output-port (lambda () (print "attributes=") (pp attributes)))
  (let* (; (data-thunk ((environment 'sack:body)))
         (sack:headers (environment 'sack:headers))
         (content-length (let ((v (sack:headers "content-length")))
                           ; (with-output-to-port console-output-port (lambda () (print "Sack: v=") (write v) (print "\n")))
                           (and (not (null? v)) (string->number (car v)))))
         (chunked-encoding (let ((v (sack:headers "transfer-encoding")))
                             ; (with-output-to-port console-output-port (lambda () (print "Sack: v=") (write v) (print "\n")))
                             (and (not (null? v)) (string-prefix? "chunked"
                                                                  (string-downcase (car v))))))
         (buf-size 4096)
         (buf #f))
    (define (init-buf!)
      (set! buf (make-u8vector buf-size)))
    (define (make-read-bytes bytes-left*)
      (lambda (data-thunk copying?)
        (let loop ((bytes-left bytes-left*) (data-thunk data-thunk) (copying? copying?))
          (if (< 0 bytes-left)
              (let* ((bytes-to-read (min buf-size bytes-left))
                     ; (_tmp (with-output-to-port console-output-port (lambda () (print "going into read " bytes-to-read " bytes.\n"))))
                     (bytes-read (read-subu8vector buf 0 bytes-to-read connection bytes-to-read)))
                ; (with-output-to-port console-output-port (lambda () (print "read " bytes-read " bytes.\n")))
                (if (and bytes-read (< 0 bytes-read))
                    (begin
                      ; (with-output-to-port console-output-port (lambda () (print "read " bytes-read " bytes.\n")))
                      (let* ((false-response? (if data-thunk
                                                 (not (if copying?
                                                          (data-thunk (subu8vector buf 0 bytes-read))
                                                          (data-thunk buf bytes-read)))
                                                 #f))
                             (continue (lambda (data-thunk copying?)
                                         (loop (- bytes-left bytes-read) data-thunk copying?))))
                        (if false-response? (box continue) continue)))
                    #f))
              #f))))
    ; (with-output-to-port console-output-port (lambda () (print "Sack: Now into reading request contents body. chunked-encoding="
    ;                                                            chunked-encoding ", content-length=" content-length 
    ;                                                            ", sack:body=" sack:body "\n")))

    (cond
     ; HTTP request has chunked encoding
     ; (End of body is denoted by a zero-length chunk.)
     (chunked-encoding
      (lambda (data-thunk copying?)
        ; (with-output-to-port console-output-port (lambda () (print "Sack: Chunked encoding is used, decoding.\n")))
        (init-buf!)
        (let loop ((data-thunk data-thunk) (copying? copying?))
          (let* ((len-str (permissive-read-line connection))
                 (len (chunked-coding-read-hex len-str)))
            (if (not (zero? len))
                (let ((read-bytes (make-read-bytes len)))
                  (let read-bytes-loop ((data-thunk data-thunk) (copying? copying?))
                    ; (with-output-to-port console-output-port (lambda () (print "reading " len " bytes http chunk\n")))
                    (let ((k (read-bytes data-thunk copying?)))
                      (if k
                          (let ((boxed? (box? k)))
                            (set! read-bytes (if boxed? (unbox k) k))
                            (if boxed? (box read-bytes-loop) read-bytes-loop)) ; (We return read-bytes-loop)
                          (begin
                            (permissive-read-line connection)
                            (loop data-thunk copying?))))))
                #f)))
        ; (with-output-to-port console-output-port (lambda () (print "read all chunks, ending.\n")))
        ))
     ; HTTP request has Content-Length set
     (content-length
      (if (not (zero? content-length)) ; No need to allocate buffer and do read op if length is zero
          (lambda (data-thunk copying?)
            (init-buf!)
            ; (with-output-to-port console-output-port (lambda () (print "Sack: Content-Length set to " content-length ", processing.\n")))
            ((make-read-bytes content-length) data-thunk copying?))
          #f))
     ; HTTP request has no Content-Length, but has Connection: Close
     ; HTTP request has neither Content-Length nor Connection: Close
     ; In this case we presume that there is no request body. (Presumably this is in accordance with the HTTP RFC?)
     (else #f)
     )

    ; Please note that the following code was written for this procedure when it had done all reading work already
    ; up to here. Now it's rather event-based. So the following code could not be taken in use straight off now.
    ; (let ((b1 (read-u8 connection)) (b2 (read-u8 connection)))
    ;   (if (not (and (memq b1 '(#\return #!eof)) (memq b2 '(#\newline #!eof))))
    ;       (begin
    ;         (with-output-to-port console-output-port (lambda () (print "Sack: Invalid end of request, expected chars 0D 0A. ")
    ;                                                    (write b1) (print " ") (write b2) (print "\n")))
    ;         (error "Invalid end of " b1 b2))))
    ; ^ I think this is commented out because the error never happened again. In case it would though
    ;   and it's because of erratic behavior from the client, 

    ; (with-output-to-port console-output-port (lambda () (print "Sack: Finished any request contents processing for connection.\n")))
    ))

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
            (force-output conn 1) ; 1 = fsync = ask OS to actually send it.
            ))
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
  ; We get here from handle-request of serve-connection . At this point, the HTTP request headers have
  ; been read in full, and none of the body has been read. The process now is to invoke the sack
  ; application and get the response HTTP headers in full from it, and then output the headers and
  ; the HTTP body based on the response-thunk 's return values, and by that the handling of this HTTP
  ; request is done, with the exception for reading the HTTP request body.
  ;      This is done by invocation to the 'sack:body environment parameter. The sack application may
  ; do this as for it to get the HTTP request body's contents. After all execution of the sack app
  ; for this request, we also run 'sack:body from here to ensure complete drainage from the HTTP
  ; connection port of the body, so that the connection is reset correctly for handling of any
  ; subsequent HTTP request in the same connection.
  ;      At the end of the handling of this request i.e. at the bottom of this procedure, 'sack:body
  ; for this request is blocked from any subsequent reading, so that just in case it'd be called
  ; by the sack app after the processing of this request has finished (by another thread etc.) it
  ; won't interfere with the handling of subsequent HTTP requests using the same connection.

  (define http-request-body-read (make-sack-request-body-handler environment conn))

  ; sack:body for the sack-application works as follows:
  ; ((environment 'sack:body) data-thunk #!key (copying? #t))
  ; If copying? is #t: data-thunk is a procedure that takes the arguments (u8v), where u8v is an
  ; u8vector containing the most recently read block/chunk of data. Sack makes no further use of u8v.
  ; If copying? is #f: data-thunk is a procedure that takes the arguments (u8v len), where u8v
  ; is an u8vector whose len first bytes contain the most recently read block/chunk of data. The
  ; data is guaranteed to be there up to and only up to that data-thunk returns.
  ;
  ; The read operation with sack:body per above returns when data-thunk returned #f or when the
  ; request body has been fully read.
  ; 
  ; sack:body returns #f if data-thunk returned #f or if the end was reached already on the previous
  ; sack:body call, otherwise #t. (Other than this, its return value is not affected by whether any
  ; data was actually read.)
  ;
  ; sack:body may be re-run at any time. If its previous return was because of end of data,
  ; the new run will just return #f . If the previous return was because data-thunk returned #f,
  ; the new sack:body run will continue reading right after the point the previous reading ended.
  (define sack:body (lambda (data-thunk #!key (copying? #t))
                      (let loop ()
                        (and http-request-body-read
                             (let* ((n (http-request-body-read data-thunk copying?))
                                    (boxed? (box? n)))
                               (set! http-request-body-read (if boxed? (unbox n) n))
                               (not boxed?))
                             (loop)))))
  ((environment 'sack:body) sack:body)

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
             (if (and (not (equal? "head"
                                   (environment 'sack:request-method)))
                      (not (eq? 304 code)))
                 (if chunked-encoding?
                     (handle-sack-response-write-chunked conn response-thunk)
                     (with-output-to-port conn respond)))

             ; In case there's any more HTTP request body that's un-read
             ; (- in this scope: not skipped over yet) up to now, do that now.
             (sack:body (lambda (u8v len) (void)) copying?: #f)
             )

            (else
             ;; Unknown HTTP version. Just dump the body.
             (with-output-to-port conn respond)))

          ; Was:
          ; (if close-connection?
          ;     (close-port conn)
          ;     (force-output conn))
          ;
          ; Just to ensure completely that the data is sent (I believe this is superfluous but let's keep it for now):
          (force-output conn 1)
          (if close-connection? (close-port conn))

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
    (if (or (not (string? req)) (equal? "" req)) ; If connection is closed, permissive-read-line returns "".
        (bad-request-error connection)           ; This happens for instance when a Keepalive connection is closed.
        (begin
          ; (with-output-to-port console-output-port (lambda () (print "Got HTTP req line: ") (write req) (print ".\n")))
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
              (with-exception/continuation-catcher
               (lambda (e)
                 (let ((exception (exception/continuation-exception e)))
                   (if (and (os-exception? exception)
                            (let ((v (os-exception-arguments exception)))
                              (and (list? v) (>= (length v) 1)
                                   (eq? connection (car v)))))

                       ; Typically this is a "Broken pipe" exception. Don't know exactly how
                       ; to typecheck for it though.
                       (print port: console-output-port
                              " #### Sack application's connection failed, IO error. " e "\n")

                       (print port: console-output-port
                              "\n\n #### Sack application crashed with exception:\n"
                              (exception/continuation->string e #f #t 100 100))) ;; TODO

                   ; Send internal server error to connection.
                   ; If we get an exception while doing that, ignore it.
                   (with-exception-catcher
                    (lambda (e) #t)
                    (lambda () (internal-server-error connection)))

                   (list 'exception e (exception/continuation->string e))))
               (lambda ()
                 ;; Add some more info to the uri object. This is
                 ;; useful for the sack environment object.
                 (let* ((host/port (let ((ret (assoc "host" attributes)))
                                     (and ret (cdr ret))))
                        (host+port (string-split-char #\: (or host/port
                                                              "0.0.0.0"))))
                   (set! uri
                         (uri-port-set uri
                                       (or (and (pair? (cdr host+port))
                                                (string->number
                                                 (cadr host+port)))
                                           port-number)))
                   (set! uri (uri-host-set uri (car host+port)))
                   (set! uri (uri-scheme-set uri "http")))

                 ; (with-output-to-port console-output-port (lambda () (print "Sack: Handles request " uri ".\n")))

                 ; This is the core handling block for each HTTP connection - the creation of an
                 ; environment for it, and the complete handling (invocation of sack app and so on).
                 (let* ((environment (make-environment threaded?
                                                       uri
                                                       method-name
                                                       attributes)))
                   (handle-sack-response
                    keep-alive
                    sack-application
                    environment
                    connection
                    version))
                 )))

            (if method-name
                (parse-uri
                 req
                 (+ end 1)
                 (string-length req)
                 #t
                 (lambda (uri i)
                   ; (with-output-to-port console-output-port (lambda () (print "Got uri: ") (write uri) (print " i " i "\n")))
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
