#lang racket

(define (sum-second-in-pair-list l)
  (cond ((null? l) 0)
        ((list? l) (+ (sum-second-in-pair-list (cdr l)) (second (car l))))))

(define (pick-from-pair-list n l)
  (cond ((null? l) null)
        ((< n (second (car l))) (first (car l)))
        (#t (pick-from-pair-list (- n (second (car l))) (cdr l)))))

(define (pick l)
  (cond ((null? l) '())
        ((list? l) (let* ([sum (sum-second-in-pair-list l)] [n (random sum)])
                     (pick-from-pair-list n l)))))

(provide sum-second-in-pair-list
         pick-from-pair-list
         pick)