(load "../../github-modules/build")

(import server
        show-exceptions
        pool-session-store
        cookie
        (std misc/exception))

;; GET / HTTP/1.1
;; Host: localhost
;; Cookie: _s=8A68D921-4553-4F94-BC60-9D6624A123AD

(define pool (make-session-pool))

(define (sack-app env)
  ((env 'sack:cookie:set!) (make-cookie "hej" "du"))
  (pp (list sess: ((env 'sack:session:get-all))))
  ((env 'sack:session:set!) "hej" "du")
  
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
       ((show-exceptions
         (cookies
          (pool-session-store
           sack-app
           pool: pool)))
        env))
     port-number: 3333))))



(lambda (env)
  (values 200
          '(("Content-Type" . "text/plain; charset=UTF-8"))
          (lambda ()
            (display "hej")
            #f)))
