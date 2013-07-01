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

; A country prefix is any alphanumeric symbol except [.0,/Q?1]
(define legal-country-prefix-chars
  (char-set-delete char-set:letter+digit #\. #\0 #\, #\/ #\Q #\? #\1))
(define (valid-country-prefix? c)
  (cond ((char? c) (char-set-contains? legal-country-prefix-chars c))
        (#t #f)))

(provide digits-from
         digit?
         letters-from
         letter?
         valid-country-prefix?)
