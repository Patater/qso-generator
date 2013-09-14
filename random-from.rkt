#lang racket

(define (random-from s)
  (cond ((null? s) null)
        (#t (list-ref s (random (length s))))))

(provide random-from)

