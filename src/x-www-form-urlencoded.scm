;;; x-www-form-urlencoded encoding and decoding.
;;;
;;; Written by Marc Feeley for http-server, made to a separate module
;;; and refactored by Per Eckerdal.
;;;
;;; Copyright (c) 2008-2009 Per Eckerdal, 2005-2007 Marc Feeley,
;;; 2010 Mikael More, All Rights Reserved.

; Exports:
; (urlencode str)
; [fill out]

(import (std misc/u8v))

(define (hex str n i)
  (if (< (+ i 1) n)
      (let ((h1 (nibble str i))
            (h2 (nibble str (+ i 1))))
        (and h1 h2 (+ (* h1 16) h2)))
      #f))

(define (nibble str i)
  (let ((c (string-ref str i)))
    (cond ((and (char>=? c #\0) (char<=? c #\9))
           (- (char->integer c) (char->integer #\0)))
          ((and (char>=? c #\a) (char<=? c #\f))
           (+ 10 (- (char->integer c) (char->integer #\a))))
          ((and (char>=? c #\A) (char<=? c #\F))
           (+ 10 (- (char->integer c) (char->integer #\A))))
          (else
           #f))))

; write ISO8859-characters string str to ISO8859-urlencoded format to current-output-port
(define (write-urlencoded-ISO8859 str)
  (define (write-nibble n)
    (write-char (##string-ref "0123456789ABCDEF" n)))
  
  (let loop ((i 0))
    (if (< i (string-length str))
        (let ((c (string-ref str i)))
          (cond ((or (and (char>=? c #\a) (char<=? c #\z))
                     (and (char>=? c #\A) (char<=? c #\Z))
                     (and (char>=? c #\0) (char<=? c #\9))
                     (char=? c #\_))
                 (write-char c))
                ((char=? c #\space)
                 (write-char #\+))
                (else
                 (let ((n (char->integer c)))
                   (write-char #\%)
                     (write-nibble
                      (bitwise-and (arithmetic-shift n -4) 15))
                     (write-nibble (bitwise-and n 15)))))
          (loop (+ i 1))))))

; urlencode ISO8859-characters string str to ISO8859-urlencoded format.
; This is not the common standard on the Internet today, but is used by some parties for instance
; with legacy non-Unicode systems.
(define (urlencode-ISO8859 str)
  (with-output-to-string
    ""
    (lambda () (write-urlencoded-ISO8859 str))))

(define (write-urlencoded-u8vector u8v)
  (define (write-nibble n)
    ; The following vector comes from
    ; (list->vector (map char->integer (string->list "0123456789ABCDEF")))
    (write-u8 (##vector-ref '#(48 49 50 51 52 53 54 55 56 57 65 66 67 68 69 70) n)))
  
  (let loop ((i 0))
    (if (< i (u8vector-length u8v))
        (let ((b (u8vector-ref u8v i)))
          (cond ((or (and (fx>= b 97) (fx<= b 122)) ; (char->integer #\a), (char->integer #\z)
                     (and (fx>= b 65) (fx<= b 90 )) ; (char->integer #\A), (char->integer #\Z)
                     (and (fx>= b 48) (fx<= b 57 )) ; (char->integer #\0), (char->integer #\9)
                     (eq? b 95) ; (char->integer #\_)
                     (eq? b 46) ; (char->integer #\.) - Microsoft (api.bing.com) doesn't handle .:s urlencoded in HTTP query key values
                     )
                 (write-u8 b))
                ((eq? b 32) ; (char->integer #\space)
                 (write-u8 (char->integer #\+)))
                (else
                 (write-u8 (char->integer #\%))
                 (write-nibble
                  (bitwise-and (arithmetic-shift b -4) 15))
                 (write-nibble (bitwise-and b 15))))
          (loop (+ i 1))))))

(define (urlencode->u8vector str)
  (with-output-to-u8vector
    '()
    (lambda () (write-urlencoded-u8vector (string->u8vector str)))))

; Performs an UTF8-urlencoding, which is the common standard on the Internet today.
(define (urlencode str)
  (u8vector->string (urlencode->u8vector str)))

; urldecode an iso8859-encoded string to string
(define (urldecode-ISO8859 str)
  (let* ((len (string-length str))
         (ret (make-string len))
         (strpos 0))
    (let loop ((i 0))
      (if (not (>= i len))
          (let ((chr (string-ref str i)))
            (if (eq? chr #\%)
                (begin
                  (string-set! ret
                               strpos
                               (integer->char (hex str len (+ i 1))))
                  (set! strpos (+ strpos 1))
                  (loop (+ i 3)))
                (begin
                  (string-set! ret strpos chr)
                  (set! strpos (+ strpos 1))
                  (loop (+ i 1)))))))
    (substring ret 0 strpos)))

; urldecode to u8vector
(define (urldecode->u8vector str)
  (let* ((len (string-length str))
         (ret (make-u8vector len))
         (u8vpos 0))
    (let loop ((i 0))
      (if (not (>= i len))
          (let ((chr (string-ref str i)))
            (if (eq? chr #\%)
                (begin
                  (u8vector-set! ret
                                 u8vpos
                                 (hex str len (+ i 1)))
                  (set! u8vpos (+ u8vpos 1))
                  (loop (+ i 3)))
                (begin
                  (u8vector-set! ret u8vpos (char->integer chr))
                  (set! u8vpos (+ u8vpos 1))
                  (loop (+ i 1)))))))
    (subu8vector ret 0 u8vpos)))

; urldecode an utf8-encoded string, to string
(define (urldecode str)
  (u8vector->string (urldecode->u8vector str)))

(define (write-x-www-form-urlencoded fields)
  (define (write-field field)
    (write-urlencoded (car field))
    (write-char #\=)
    (write-urlencoded (cdr field)))
  
  (if (not (null? fields))
      (begin
        (let ((field1 (car fields)))
          (write-field field1)
          (for-each (lambda (field)
                      (write-char #\&)
                      (write-field field))
                    (cdr fields))))))

(define (encode-x-www-form-urlencoded fields)
  (if (null? fields)
      ""
      (with-output-to-string
        ""
        (lambda ()
          (write-x-www-form-urlencoded fields)))))

(define decode-x-www-form-urlencoded
  (lambda (str)
    (let ((n (string-length str)))

      (define extract
        (lambda (start len)
          (let ((s (make-string len)))
            (let loop ((i start) (j 0))
              (if (< j len)
                  (let ((c (string-ref str i)))
                    (cond ((char=? c #\%)
                           (cond ((hex (+ i 1))
                                  =>
                                  (lambda (x)
                                    (string-set! s j (integer->char x))
                                    (loop (+ i 3) (+ j 1))))
                                 (else
                                  #f)))
                          ((char=? c #\+)
                           (string-set! s j #\space)
                           (loop (+ i 1) (+ j 1)))
                          (else
                           (string-set! s j c)
                           (loop (+ i 1) (+ j 1)))))
                  s)))))

      (define hex
        (lambda (i)
          (if (< (+ i 1) n)
              (let ((h1 (nibble i))
                    (h2 (nibble (+ i 1))))
                (and h1 h2 (+ (* h1 16) h2)))
              #f)))

      (define nibble
        (lambda (i)
          (let ((c (string-ref str i)))
            (cond ((and (char>=? c #\0) (char<=? c #\9))
                   (- (char->integer c) (char->integer #\0)))
                  ((and (char>=? c #\a) (char<=? c #\f))
                   (+ 10 (- (char->integer c) (char->integer #\a))))
                  ((and (char>=? c #\A) (char<=? c #\F))
                   (+ 10 (- (char->integer c) (char->integer #\A))))
                  (else
                   #f)))))

      (define state0 ; at beginning of string
        (lambda (i rev-fields)
          (if (< i n)
              (state1 i
                      i
                      0
                      rev-fields)
              (reverse rev-fields))))

      (define state1 ; in field name
        (lambda (i start len rev-fields)
          (if (< i n)
              (let ((c (string-ref str i)))
                (cond ((char=? c #\=)
                       (state2 (+ i 1)
                               (+ i 1)
                               0
                               (extract start len)
                               rev-fields))
                      ((char=? c #\%)
                       (and (hex (+ i 1))
                            (state1 (+ i 3)
                                    start
                                    (+ len 1)
                                    rev-fields)))
                      (else
                       (state1 (+ i 1)
                               start
                               (+ len 1)
                               rev-fields))))
              #f)))

      (define state2 ; in field value
        (lambda (i start len name rev-fields)

          (define end-of-field
            (lambda ()
              (cons (cons name (extract start len))
                    rev-fields)))

          (if (< i n)
              (let ((c (string-ref str i)))
                (cond ((char=? c #\&)
                       (state1 (+ i 1)
                               (+ i 1)
                               0
                               (end-of-field)))
                      ((char=? c #\%)
                       (and (hex (+ i 1))
                            (state2 (+ i 3)
                                    start
                                    (+ len 1)
                                    name
                                    rev-fields)))
                      (else
                       (state2 (+ i 1)
                               start
                               (+ len 1)
                               name
                               rev-fields))))
              (reverse (end-of-field)))))

      (state0 0 '()))))
