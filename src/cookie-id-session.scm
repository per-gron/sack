;;; Tools for making session middleware that rely on putting the
;;; session id in a cookie.
;;;
;;; Copyright (c) 2008-2009 Per Eckerdal

;;   API for code that uses sessions:
;; (session-regenerate-id!)
;;
;;   API for session sack middleware:
;; (session-generate-id)
;; (with-cookie-id-session-store
;;  get destroy! commit! thunk [generate-id] [cookie-name])
;;
;; the get function will be called at most once, with a session id as
;; parameter. It should return a hash table.
;;
;; the destroy! function will be called at most once, always after
;; thunk has returned, and never with a session id other than that sent
;; to get.
;;
;; the commit! function will be called at most once, always after
;; thunk has returned. commit! will always be called if get has been
;; called and not destroy!. It takes an id and a hash table as
;; parameters.
;;
;; What about re-entering of thunk? Disallowed, right?

(import (std misc/uuid)
        cookie
        session)

(export session-regenerate-id!

        session-generate-id
        with-cookie-id-session-store)

(define (session-regenerate-id!)
  ((session-store-extra-data)))

(define (session-generate-id)
  (make-uuid))

(define (with-cookie-id-session-store thunk
                                      #!key
                                      get
                                      destroy!
                                      commit!
                                      (generate-id session-generate-id)
                                      (cookie-name "_s"))
  ...)







(define (make-session-store #!key
                            get
                            create!
                            destroy!
                            (regenerate-id! (lambda (id) id))
                            fetch
                            store!
                            delete!)
  (if (not (and get create! destroy! regenerate-id! fetch store! delete!))
      (error "Not all required parameters were supplied"))

  (make-session-store-internal
   get create! destroy! regenerate-id! fetch store! delete!))

;; This is a data structure that is only used within this module.
(define-type session-conf
  id: 87A040CC-5F66-42D3-A023-820FEE186313

  cookie-name

  (get read-only:)
  (create! read-only:)
  (destroy! read-only:)
  (regenerate-id! read-only:)
  (fetch read-only:)
  (store! read-only:)
  (delete! read-only:))

(define session-conf (make-parameter #f))

(define (get-session-conf)
  (or (session-conf)
      (error "Not in a session dynamic environment")))

(define (make-counter)
  (let ((num 0))
    (lambda ()
      (set! num (+ num 1))
      num)))

(define session-variable-counter (make-counter))
(define session-variable-nochange (gensym))

(define (session-set-cookie! session-conf id)
  (cookie-set! (session-conf-cookie-name
                session-conf)
               id
               path: "/"))

;; Gets the current session id
(define (session-id session-conf)
  (cookie-get (session-conf-cookie-name
               session-conf)))

;; Opens the current session, or creates a new one if one doesn't
;; exist.
(define (session-open conf)
  (or ((session-conf-get data) (session-id conf))
      (let ((id/session-pair ((session-conf-create! conf))))
        (session-set-cookie! conf (car id/session-pair))
        (cdr id/session-pair))))


(define (make-session-variable #!optional default-val)
  (if (not default-val)
      (set! default-val (lambda () #f)))
  
  (let ((id (session-variable-counter)))
    (lambda (#!optional (set-to session-variable-nochange))
      (let ((conf (session-conf)))
        (if (eq? set-to session-variable-nochange)
            (if (not conf)
                (default-val)
                (let ((sess (session-open conf)))
                  ((session-conf-fetch conf)
                   sess
                   id
                   (lambda (val)
                     val)
                   (lambda ()
                     (let ((v (default-val)))
                       ((session-conf-store! sess id v))
                       v)))))
            (let ((sess
                   (if conf
                       (session-open conf)
                       (error "Not in a session dynamic environment"))))
              ((session-conf-store! conf)
               sess id set-to)))))))

(define (session-generate-id)
  (make-uuid))

;; Regenerates the session id. This is here for security reasons;
;; a session id that changes on important occasions like login will
;; be more difficult to hijack
(define (session-regenerate-id!)
  (let* ((session-conf (get-session-conf))
         (old-id (session-id session-conf))
         (new-id (session-conf-regenerate-id! old-id)))
    (session-set-cookie! session-conf new-id)))

;; Destroys a session. Does nothing if a session doesn't exist.
(define (session-destroy!)
  (let ((session-conf (get-session-conf))
        (id (session-id session-conf)))
    (if id
        (begin
          (session-conf-destroy! id)
          (cookie-del! (session-conf-cookie-name
                        session-conf))))))

(define (session-sack store
                      app
                      #!key
                      (cookie-name "_s"))
  (let ((conf (make-session-conf
               cookie-name
               (session-store-get store)
               (session-store-create! store)
               (session-store-destroy! store)
               (session-store-regenerate-id! store)
               (session-store-fetch store)
               (session-store-store! store)
               (session-store-delete! store))))
    (lambda (env)
      (parameterize
       ((session-conf conf))
       (app env)))))
