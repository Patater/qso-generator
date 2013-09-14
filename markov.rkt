#lang racket
(require "random-from.rkt")

(define (update-n-gram-hash! n s h position)
    (cond ((< position (- n 1)) '())
          (#t (let ([key (n-gram-at n position s)])
                (cond ((hash-has-key? h key)
                        (hash-set! h key (+ (hash-ref h key) 1)))
                      (#t (hash-set! h key 1)))
                (update-n-gram-hash! n s h (- position 1))))))

(define (n-gram-at n position s)
  (cond ((< n 1) '())
        (#t (cons (list-ref s (- (+ position 1) n))
                  (n-gram-at (- n 1) position s)))))

(define (n-grams n s)
  (let ([position (- (length s) 1)] [h (make-hash)])
    (update-n-gram-hash! n s h position)
    h))

(provide n-gram-at
         n-grams)
