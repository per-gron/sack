(load "../../github-modules/build")

(import server
        show-exceptions
        cookie
        (std misc/exception))

(define (sack-catch-errors app)
    (lambda (env)
      (with-exception/continuation-catcher
       (lambda (e)
         (values 500
                 '(("Content-Type" . "text/plain; charset=UTF-8"))
                 (lambda ()
                   (display "500 Internal Server Error\n\n")
                   (display
                    (exception/continuation->string e))
                   #f)))
       (lambda ()
         (app env)))))

(define (sack-app env)
  (cookie-set! "hej" "du")
  
  (values 200
          '(("Content-Type" . "text/plain; charset=UTF-8")
            ;;("Content-Length" . "3")
            ;;("Connection" . "close")
            )
          (lambda ()
            (display "hej")
            #f)))

(thread-start!
 (make-thread
  (lambda ()
    (http-server-start!
     (lambda (env)
       ((show-exceptions-sack
         (cookies-sack sack-app))
        env))
     port-number: 3333))))



(lambda (env)
  (values 200
          '(("Content-Type" . "text/plain; charset=UTF-8"))
          (lambda ()
            (display "hej")
            #f)))
