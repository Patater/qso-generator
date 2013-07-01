#lang racket

(require srfi/14) ; char-set-contains

; Return a list containing only the digits in the supplied list, maintaining
; order.
(define (digits-from s)
  (filter digit? s))

; Evaluate to true iff the supplied character is a digit.
; If a character is not supplied, evaluate to false.
(define (digit? c)
  (cond ((char? c) (char-set-contains? char-set:digit c))
        (#t #f)))

; Return a list containing only the letters in the supplied list, maintaining
; order.
(define (letters-from s)
  (filter letter? s))

; Evaluate to true iff the supplied character is a letter.
; If a character is not supplied, evaluate to false.
(define (letter? c)
  (cond ((char? c) (char-set-contains? char-set:letter c))
        (#t #f)))

; Return a list containing only the letters and digits in the supplied list,
; maintaining order.
(define (letters+digits-from s)
    (filter letter+digit? s))

; Evaluate to true iff the supplied character is a letter or a digit.
; If a character is not supplied, evaluate to false.
(define (letter+digit? c)
    (cond ((char? c) (char-set-contains? char-set:letter+digit c))
                  (#t #f)))

; A country prefix is any alphanumeric symbol except [.0,/Q?1]
(define legal-country-prefix-chars
  (char-set-delete char-set:letter+digit #\. #\0 #\, #\/ #\Q #\? #\1))
(define (valid-country-prefix? c)
  (cond ((char? c) (char-set-contains? legal-country-prefix-chars c))
        (#t #f)))

(define (minimum-country-prefix-length c)
  (let ([min-prefix-length-2 '(#\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\C #\D #\E #\H
                               #\J #\L #\O #\P #\S #\T #\U #\V #\X #\Y #\Z)]
        [min-prefix-length-1 '(#\2 #\B #\F #\G #\I #\K #\M #\N #\R #\W)])
    (cond ((member c min-prefix-length-2) 2)
          ((member c min-prefix-length-1) 1)
          (#t -1))))

(define (allow-numeral-in-country-prefix? c)
  (let ([numeral-allowed '(#\A #\C #\D #\E #\H #\J #\L
                           #\P #\S #\T #\V #\Y #\Z)])
  (cond ((member c numeral-allowed) #t)
        (#t #f))))

(provide digits-from
         digit?
         letters-from
         letter?
         letters+digits-from
         letter+digit?
         valid-country-prefix?
         minimum-country-prefix-length
         allow-numeral-in-country-prefix?)
