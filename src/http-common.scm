;;; Routines common to both a HTTP client and server.
;;;
;;; Copyright (c) 2008-2009 Per Eckerdal, 2005-2007 Marc Feeley, All
;;; Rights Reserved.

(import (std string/util
             srfi/1
             srfi/13)
        (only: (std srfi/19) date->string)
        x-www-form-urlencoded)

(export http-status-code
        date->rfc1123
        display-crlf
        display-header
        display-headers
        split-attribute-line
        permissive-read-line-reached-eof
        permissive-read-line
        permissive-read-lines
        read-header
        read-content-chars
        chunked-coding-read-hex)

(declare (block)
         (mostly-fixnum)
         (standard-bindings)
         (extended-bindings))

;==============================================================================

; Status codes.

(define http-status-code
  (let ((http-status-codes (make-table)))
    (for-each (lambda (x)
                (table-set! http-status-codes
                            (car x)
                            (cdr x)))
              '((100 . "Continue")
                (101 . "Switching Protocols")
                (200 . "OK")
                (201 . "Created")
                (202 . "Accepted")
                (203 . "Non-Authoritative Information")
                (204 . "No Content")
                (205 . "Reset Content")
                (206 . "Partial Content")
                (300 . "Multiple Choices")
                (301 . "Moved Permanently")
                (302 . "Found")
                (303 . "See Other")
                (304 . "Not Modified")
                (305 . "Use Proxy")
                (307 . "Temporary Redirect")
                (400 . "Bad Request")
                (401 . "Unauthorized")
                (402 . "Payment Required")
                (403 . "Forbidden")
                (404 . "Not Found")
                (405 . "Method Not Allowed")
                (406 . "Not Acceptable")
                (407 . "Proxy Authentication Required")
                (408 . "Request Timeout")
                (409 . "Conflict")
                (410 . "Gone")
                (411 . "Length Required")
                (412 . "Precondition Failed")
                (413 . "Request Entity Too Large")
                (414 . "Request-URI Too Long")
                (415 . "Unsupported Media Type")
                (416 . "Requested Range Not Satisfiable")
                (417 . "Expectation Failed")
                (500 . "Internal Server Error")
                (501 . "Not Implemented")
                (502 . "Bad Gateway")
                (503 . "Service Unavailable")
                (504 . "Gateway Timeout")
                (505 . "HTTP Version Not Supported")))
    (lambda (num)
      (table-ref http-status-codes num))))

;==============================================================================

; Header writing functions.

(define (write-u8vector vec #!optional port)
  (write-subu8vector vec
                     0
                     (u8vector-length vec)
                     (or port (current-output-port))))

(define (date->rfc1123 d)
  (date->string d "~a, ~d ~b ~Y ~T GMT"))

(define (display-crlf port . msgs)
  (if (not (null? msgs))
      (for-each (lambda (msg)
                  (display msg port))
                msgs))
  (display "\r\n" port))

(define (display-header port pair)
  (display-crlf port (car pair) ": " (cdr pair)))

(define (display-headers port hs)
  (for-each (lambda (x)
              (display-header port x))
            hs))

;==============================================================================

; Http reading functions.

(define (find-char-pos str char)
  (let loop ((i 0))
    (if (< i (string-length str))
        (if (char=? char (string-ref str i))
            i
            (loop (+ i 1)))
        #f)))

(define (split-attribute-line line)
  (let ((pos (find-char-pos line #\:)))
    (and pos
         (< pos (string-length line))
         (cons (let ((str (substring line 0 pos)))
                 (string-downcase! str)
                 str)
               (string-strip
                (substring line (+ pos 1) (string-length line)))))))

(define permissive-read-line-reached-eof
  (list 'permissive-read-line-reached-eof))

;; Reads one line from port, using the byte reading functions and not
;; read-line. Safe to US ASCII only.
(define (permissive-read-line port #!optional exception-on-eof?)
  (let loop ((lst '()))
    (let ((x (read-u8 port)))
      (cond
       ((and (eq? x 10)
             (pair? lst)
             (eq? (car lst) #\return))
        (reverse-list->string (cdr lst)))

       ((eq? #!eof x)
        (if exception-on-eof?
            (raise permissive-read-line-reached-eof)
            (reverse-list->string lst)))

       (else
        (loop (cons (integer->char x) lst)))))))

(define (permissive-read-lines port)
  (let loop ()
    (let ((line (permissive-read-line port #t)))
      (if (zero? (string-length line))
          (loop)
          line))))

;; Reads one line from port, using the byte reading functions and not
;; read-line. Safe to US ASCII only. This function also treats a CRLF
;; followed by a space or a tab as whitespace, not a newline.
;;
;; Used when parsing headers.
(define (permissive-read-line-skip-lws port peek-char)
  (let loop ((lst '())
             (current-char peek-char)
             (peek-byte (read-u8 port)))
    (cond
     ((and (null? lst)
           (eq? 10 peek-byte) ;; #\newline
           (eq? #\return current-char))
      (cons 'done ""))
     
     ((eq? #!eof peek-byte)
      (cons #!eof
            (reverse-list->string
             (if (and (eq? #\newline current-char)
                      (pair? lst)
                      (eq? #\return (car lst)))
                 (cdr lst)
                 (cons current-char lst)))))

     ((and (eq? current-char #\newline)
           (pair? lst)
           (eq? (car lst) #\return)
           (not (or (eq? 9 peek-byte) ;; #\tab
                    (eq? 32 peek-byte)))) ;; #\space
      (cons peek-byte
            (reverse-list->string (cdr lst))))
      
      (else
       (let ((chr (integer->char peek-byte)))
         (loop (cons current-char lst)
               chr
               (read-u8 port)))))))

(define (read-header port)
  (let loop ((attributes '())
             (peek-byte (read-u8 port)))
    (and (not (eq? #!eof peek-byte))
         (let* ((peek-byte/line-pair
                 (permissive-read-line-skip-lws
                  port
                  (integer->char peek-byte)))
                (new-peek-byte
                 (car peek-byte/line-pair))
                (line
                 (cdr peek-byte/line-pair)))
           (cond
            ((eq? 'done new-peek-byte)
             attributes)

            ((eq? #!eof new-peek-byte)
             #f)

            (else
             (let ((attribute (split-attribute-line line)))
               (if attribute
                   (loop (cons attribute attributes)
                         new-peek-byte)
                   #f))))))))

(define (read-content-chars port attributes)
  (let ((cl
         (cond ((assoc "content-length" attributes)
                =>
                (lambda (x)
                  (let ((n (string->number (cdr x))))
                    (and n (integer? n) (exact? n) n))))
               (else
                #f))))
    (if cl
        (let ((str (make-string cl)))
          (let ((n (read-substring str 0 cl port)))
            (if (= n cl)
                str
                "")))
        "")))

(define (chunked-coding-read-hex str)
  (let* ((str-len (string-length str))
         (chr-lst
          (let loop ((lst '()) (idx 0))
            (cond
             ((or (>= idx str-len)
                  (let ((chr (string-ref str idx)))
                    (or (char=? #\; chr)
                        (char=? #\space chr))))
              lst)

             (else
              (loop (cons (char->integer
                           (string-ref str idx))
                          lst)
                    (+ 1 idx))))))
         (zero (char->integer #\0))
         (nine (char->integer #\9))
         (a (char->integer #\a))
         (A (char->integer #\A))
         (f (char->integer #\f))
         (F (char->integer #\F)))
    (let loop ((lst chr-lst) (multiple 1))
      (if (null? lst)
          0
          (let ((chr (car lst)))
            (+ (loop (cdr lst) (* multiple 16))
               (* multiple
                  (cond
                   ((and (>= chr zero)
                         (<= chr nine))
                    (- chr zero))

                   ((and (>= chr a)
                         (<= chr f))
                    (+ 10 (- chr a)))

                   ((and (>= chr A)
                         (<= chr F))
                    (+ 10 (- chr A)))

                   (else
                    (error "Invalid character in hex string" str))))))))))
