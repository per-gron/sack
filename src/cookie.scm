;;; Cookie sack middleware.
;;;
;;; Copyright (c) 2008-2009 Per Eckerdal

(import http-common)

(export cookie-sack
        cookie-set!
        cookie-ref
        cookie-del!
        cookie-get

        (re-export:
         (only: http-common
                cookie?
                cookie-name
                cookie-value
                cookie-expires
                cookie-domain
                cookie-path
                cookie-port
                cookie-secure
                cookie-http-only)))

(define-type cookie-data
  id: 9B3025BE-F7EB-4010-840A-E3CEB8F46BDD

  ;; Both these values are lazily initialized
  (cookie-changes init: #f)
  (cookies read-only:))

(define cookie-data (make-parameter #f))

;; Lazily get the cookie changes table
(define (cookie-get-changes)
  (let ((data (cookie-data)))
    (if (not data)
        (error "Not in cookie dynamic environment"))
    
    (or (cookie-data-cookie-changes data)
        (let ((tbl (make-table)))
          (cookie-data-cookie-changes-set! data tbl)
          tbl))))

(define (cookie-get-cookies)
  (let ((data (cookie-data)))
    (if (not data)
        (error "Not in cookie dynamic environment"))
    
    (cookie-data-cookies data)))

(define (cookie-set! name value #!key expires domain path port secure http-only)
  (let ((chs (cookie-get-changes)))
    (table-set! chs name (make-cookie name
                                      value
                                      expires: expires
                                      domain: domain
                                      path: path
                                      port: port
                                      secure: secure
                                      http-only: http-only))))

(define (cookie-ref name)
  (let* ((chs (cookie-get-changes))
         (cs (cookie-get-cookies)))
    (or (table-ref chs name #f)
        (table-ref cs name #f))))

(define (cookie-del! name)
  (cookie-set! name #f expires: date-in-the-past))

(define (cookie-get name)
  (let ((c (cookie-ref name)))
    (if c (cookie-value c) #f)))

(define (cookie-sack app)
  (lambda (env)
    (parameterize
     ((cookie-data
       (make-cookie-data
        (let ((cookie-tbl (make-table)))
          (for-each
           (lambda (cookie-header)
             (for-each (lambda (cookie)
                         (table-set! cookie-tbl
                                     (cookie-name cookie)
                                     cookie))
                       (cookie-parse-to-list cookie-header)))
           ((env 'sack:headers) "cookie"))
          cookie-tbl))))
     (call-with-values
         (lambda ()
           (app env))
       (lambda (code headers respond)
         (let ((changes
                (cookie-data-cookie-changes
                 (cookie-data))))
           (if changes
               (table-for-each
                (lambda (key val)
                  (set! headers
                        (cons (cons "Set-Cookie" (cookie-to-http val))
                              headers)))
                changes)))
         
         (values code headers respond))))))

