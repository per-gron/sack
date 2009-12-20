;;; Cookie sack middleware.
;;;
;;; Copyright (c) 2008-2009 Per Eckerdal

(import http-common)

(export cookies
        
        (re-export:
         (only: http-common
                make-cookie
                cookie?
                cookie-name
                cookie-value
                cookie-expires
                cookie-domain
                cookie-path
                cookie-port
                cookie-secure
                cookie-http-only

                date-in-the-past)))

(define (cookies app)
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

