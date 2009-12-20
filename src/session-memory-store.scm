;;; Sack session store that stores the session data in memory.
;;;
;;; Copyright (c) 2008-2009 Per Eckerdal

(import session)

(export make-session-memory-store)

(define not-found (list 'not-found))

(define (make-session-memory-store)
  (let ((sessions (make-table)))
    (make-session-store
     get:
     (lambda (id found not-found)
       (let ((ret (table-ref sessions id not-found)))
         (if (eq? ret not-found)
             (not-found)
             (found ret))))

     create!:
     (lambda ()
       (let ((id (session-generate-id))
             (sess (make-table)))
         (table-set! sessions id sess)
         (cons id sess)))

     destroy!:
     (lambda (id)
       (table-set! sessions id))

     regenerate-id!:
     (lambda (old-id)
       (let ((new-id (session-generate-id))
             (sess (table-ref sessions old-id not-found)))
         (if (not (eq? sess not-found))
             (begin
               (table-set! sessions old-id)
               (table-set! sessions new-id sess)))))

     fetch:
     (lambda (sess name found not-found)
       (let ((ret (table-ref sess name not-found)))
         (if (eq? ret not-found)
             (not-found)
             (found ret))))

     store!:
     (lambda (sess name val)
       (table-set! sess name val))

     delete!:
     (lambda (sess name)
       (table-set! sess name)))))
