;;; Cookie sack middleware.
;;;
;;; Copyright (c) 2008-2009 Per Eckerdal

(import (srfi time
              strings)
        (only: (srfi lists) find)
        (only: #(std string/util)
               string-strip
               string-split)
        http-common
        x-www-form-urlencoded)

(export make-cookie
        cookie?
        cookie-name
        cookie-value
        cookie-value-set!
        cookie-expires
        cookie-expires-set!
        cookie-domain
        cookie-domain-set!
        cookie-path
        cookie-path-set!
        cookie-port
        cookie-port-set!
        cookie-secure
        cookie-secure-set!
        cookie-http-only
        cookie-http-only-set!
        
        date-in-the-past

        cookies->)

(define-type cookie
  id: D00E6EE6-5E55-47F2-B901-4DECBB3AA011
  constructor: make-cookie/no-check
  
  (name read-only:);; A string. Must be a valid cookie name as defined in the spec
  value ;; A string
  expires ;; A date, as in SRFI 19, or #f
  domain ;; A string or #f
  path ;; A string or #f
  port ;; A string or #f (?)
  secure ;; A boolean
  http-only) ;; A boolean

(define http-separators
  (let ((lst '()))
    (string-for-each (lambda (x)
                       (set! lst (cons x lst)))
                     "()<>@,;:\\\"/[]?={} \t")
    lst))

;; One optimization might be to simply not call this function
(define (valid-cookie-name? name)
  (call/cc
   (lambda (ret)
     (string-for-each
      (lambda (chr)
        (let ((int (char->integer chr)))
          (if (or (<= int 31)
                  (>= int 127)
                  (find (lambda (x) (eq? x chr))
                        http-separators))
              (ret #f))))
      name)
     (ret #t))))

(define (make-cookie name value #!key expires domain path port secure http-only)
  (if (not (valid-cookie-name? name))
      (error "Invalid cookie name:" name))
  (make-cookie/no-check name value expires domain path port secure http-only))

(define (cookie-to-http c)
  (let ((name (cookie-name c))
        (value (cookie-value c))
        (expires (cookie-expires c))
        (domain (cookie-domain c))
        (path (cookie-path c))
        (port (cookie-port c))
        (secure (cookie-secure c))
        (http-only (cookie-http-only c)))
    (apply string-append
           `(,name
             "="
             ,value
             
             ,@(if expires
                   `("; expires=" ,(date->rfc1123 expires))
                   '())
             ,@(if domain
                   `("; domain=" ,domain)
                   '())
             ,@(if path
                   `("; path=" ,path)
                   '())
             ,@(if port
                   `("; port="
                     ,@(cond
                        ((number? port)
                         `("\"" ,(number->string port) "\""))
                        ((pair? port)
                         `("\""
                           ,@(string-join "," (map number->string port))
                           "\""))
                        (else `(,port)))))
             ,(if secure "; secure" "")
             ,(if http-only "; HttpOnly" "")
             "; Version=1"))))

;; Takes the raw Cookie: field data and splits it into a list
;; of key/value pairs.
(define (cookie-parse-split data)
  (let ((cookies (make-table)))
    (map (lambda (s)
           (let ((sp (string-split #\= s)))
             (if (or (null? sp)
                     (null? (cdr sp)))
                 (cons "" "")
                 (cons (urldecode (car sp))
                       (urldecode (cadr sp))))))
         (map string-strip (string-split #\; data)))))

(define (cookie-parse-to-list data)
  (let ((ps (cookie-parse-split data))
        (current-cookie #f)
        (default-prefs '()) ;; The special attributes specified before
                            ;; any other cookie
        (cookies '())) ;; List of processed cookies
    (define (set-pref name val)
      (let ((name (string-downcase name)))
        (if current-cookie
            (cond
             ((equal? name "$path")
              (cookie-path-set! current-cookie val))
             ((equal? name "$domain")
              (cookie-domain-set! current-cookie val))
             ((equal? name "$port")
              (cookie-port-set! current-cookie val)))
            (set! default-prefs (cons (cons name val)
                                      default-prefs)))))
    (define (new-cookie name val)
      (if current-cookie
          (set! cookies (cons current-cookie cookies)))
      (set! current-cookie (make-cookie name val))
      (for-each (lambda (x) (set-pref (car x) (cdr x)))
                default-prefs))
    (for-each
     (lambda (pair)
       (let ((name (car pair))
             (val (cdr pair)))
         (if (not (= 0 (string-length name)))
             (if (eq? #\$ (string-ref name 0))
                 (set-pref name val)
                 (new-cookie name val)))))
     ps)
    (if current-cookie
        (cons current-cookie cookies)
        '())))

;; Parses the value of a Cookie: header and returns it as a
;; table with the cookies, where the keys are the cookie names.
(define (cookie-parse data)
  (let ((tbl (make-table)))
    (for-each (lambda (c)
                (table-set! tbl (cookie-name c) c))
              (cookie-parse-to-list data))
    tbl))

(define (cookie-headers tbl port)
  (table-for-each
   (lambda (key val)
     (display-crlf port "Set-Cookie: " (cookie-to-http val)))
   tbl))

(define date-in-the-past (make-date 0 0 0 0 1 1 1990 0))

(define (cookies-> app)
  (lambda (env)
    (let* ((cookies
            (let ((cookie-tbl (make-table)))
              (for-each
               (lambda (cookie-header)
                 (for-each (lambda (cookie)
                             (table-set! cookie-tbl
                                         (cookie-name cookie)
                                         cookie))
                           (cookie-parse-to-list cookie-header)))
               ((env 'sack:headers) "cookie"))
              cookie-tbl))
           
           (changes
            (make-table))

           (cookie-get-all
            (lambda ()
              (let ((ret '()))
                (table-for-each
                 (lambda (key value)
                   (set! ret (cons value ret)))
                 cookies)
                ret)))
           
           (cookie-get
            (lambda (name)
              (or (table-ref changes name #f)
                  (table-ref cookies name #f))))

           (cookie-set!
            (lambda (cookie)
              (let ((name (cookie-name cookie)))
                (table-set! changes name cookie)
                (table-set! cookies name cookie)))))
      
      (call-with-values
          (lambda ()
            (app
             (lambda (arg)
               (case arg
                 ((sack:cookie:get-all) cookie-get-all)
                 ((sack:cookie:get) cookie-get)
                 ((sack:cookie:set!) cookie-set!)
                 (else (env arg))))))
        (lambda (code headers respond)
          (table-for-each
           (lambda (key val)
             (set! headers
                   (cons (cons "Set-Cookie" (cookie-to-http val))
                         headers)))
           changes)
          
          (values code headers respond))))))

