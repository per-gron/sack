(import (std misc/exception))

(define (show-exceptions-sack app)
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

