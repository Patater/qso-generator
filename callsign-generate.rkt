#lang racket

(require "random-from.rkt")
(require srfi/14) ; char-set-contains
(require srfi/1) ; lset-intersection

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

(define (minimum-country-prefix-length-list n)
  (cond ((= n 2) '(#\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\C #\D #\E #\H
                   #\J #\L #\O #\P #\S #\T #\U #\V #\X #\Y #\Z))
        ((= n 1) '(#\2 #\B #\F #\G #\I #\K #\M #\N #\R #\W))
        (#t '())))

(define (minimum-country-prefix-length c)
  (cond ((member c (minimum-country-prefix-length-list 2)) 2)
        ((member c (minimum-country-prefix-length-list 1)) 1)
        (#t -1)))

(define (allow-numeral-in-country-prefix? c)
  (let ([numeral-allowed '(#\A #\C #\D #\E #\H #\J #\L
                           #\P #\S #\T #\V #\Y #\Z)])
  (cond ((member c numeral-allowed) #t)
        (#t #f))))

(define (additional-country-symbol mastered-symbols numeral-allowed)
  (if numeral-allowed (random-from (letters+digits-from mastered-symbols))
      (random-from (letters-from mastered-symbols))))

(define (add-additional-country-symbol mastered-symbols
                                       numeral-allowed
                                       remaining)
  (cond ((zero? remaining) '())
        (#t (cons (additional-country-symbol mastered-symbols numeral-allowed)
                  (add-additional-country-symbol mastered-symbols
                                                 numeral-allowed
                                                 (- remaining 1))))))

; Generate a list of valid start symbols that have a minimum prefix length less
; than or equal to the desired length.
(define (country-prefix-start-symbols desired-length mastered-symbols)
  (cond ((= desired-length 1) (lset-intersection equal?
                                (minimum-country-prefix-length-list 1)
                                mastered-symbols))
        (#t (filter valid-country-prefix? mastered-symbols))))

(define (random-country-prefix-start desired-length mastered-symbols)
  (random-from (country-prefix-start-symbols desired-length mastered-symbols)))

; Generate a valid country prefix for use in an amateur radio callsign
(define (generate-country desired-length mastered-symbols)
  (let* ([start (random-country-prefix-start desired-length mastered-symbols)]
         [numeral-allowed (allow-numeral-in-country-prefix? start)]
         [min-prefix-length (minimum-country-prefix-length start)]
         [remaining (max min-prefix-length desired-length)])
    (cond ((< min-prefix-length 0) '())
          ((< desired-length min-prefix-length) '())
          (#t (cons start (add-additional-country-symbol
                             mastered-symbols
                             numeral-allowed
                             (- (max min-prefix-length desired-length) 1)))))))

(define (in-range? c start end)
  (cond ((and (char>=? c start) (char<=? c end)) #t)
        (#t #f)))

(define (bahamas-callsign? s)
  (cond ((< (length s) 3) #f)
        ((and (equal? (take s 2) '(#\C #\6)) (in-range? (third s) #\A #\Z)) #t)
        (#t #f)))

(define (generate-separating-numeral country mastered-symbols)
  (cond ((bahamas-callsign? country) '())
        (#t (let ([digits (digits-from mastered-symbols)])
              (if (empty? digits) '() (list (random-from digits)))))))

(define (random-suffix-length)
  (let ([r (random 6)])
    (cond ((< r 1) 4) ; 1/6 of the time
          ((< r 2) 1) ; 1/6 of the time
          ((< r 3) 2) ; 1/6 of the time
          (#t 3)))) ; half the time

(define (generate-suffix length mastered-symbols)
  (cond ((zero? length) '())
        ((= length 1) (let ([letters (letters-from mastered-symbols) ])
                        (if (empty? letters) '()
                            (list (random-from letters)))))
        (#t (cons (random-from (letters+digits-from mastered-symbols))
                  (generate-suffix (- length 1) mastered-symbols)))))

(define (generate-random-callsign mastered-symbols)
  (let ([country (generate-country (random 3) mastered-symbols)])
    (append country
            (generate-separating-numeral country mastered-symbols)
            (generate-suffix (random-suffix-length) mastered-symbols))))

(provide digits-from
         digit?
         letters-from
         letter?
         letters+digits-from
         letter+digit?
         valid-country-prefix?
         minimum-country-prefix-length
         allow-numeral-in-country-prefix?
         random-from
         generate-country
         in-range?
         bahamas-callsign?
         generate-country
         generate-separating-numeral
         generate-suffix
         generate-random-callsign)
