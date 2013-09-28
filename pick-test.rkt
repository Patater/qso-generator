#lang racket
(require rackunit "pick.rkt")

(check-equal? (sum-second-in-pair-list '(("a" 2) ("b" 3) ("c" 5))) 10)

(let ([pair-list '(("a" 2) ("b" 3) ("c" 5))])
  (check-equal? (pick-from-pair-list 0 pair-list) "a")
  (check-equal? (pick-from-pair-list 1 pair-list) "a")
  (check-equal? (pick-from-pair-list 2 pair-list) "b")
  (check-equal? (pick-from-pair-list 3 pair-list) "b")
  (check-equal? (pick-from-pair-list 4 pair-list) "b")
  (check-equal? (pick-from-pair-list 5 pair-list) "c")
  (check-equal? (pick-from-pair-list 6 pair-list) "c")
  (check-equal? (pick-from-pair-list 7 pair-list) "c")
  (check-equal? (pick-from-pair-list 8 pair-list) "c")
  (check-equal? (pick-from-pair-list 9 pair-list) "c")
  (check-equal? (pick-from-pair-list 0 '((("a" "b") 1) (("c" "d") 1))) '("a" "b")))

(let ([pair-list '(() () ())])
  (check-equal? (pick-from-pair-list 0 pair-list) '()))

(let ([pair-list '(("a" 2) () ())])
  (check-equal? (pick-from-pair-list 0 pair-list) "a")
  (check-equal? (pick-from-pair-list 1 pair-list) "a"))

(let ([pair-list '(("a" 2) () ("c" 1))])
  (check-equal? (pick-from-pair-list 0 pair-list) "a")
  (check-equal? (pick-from-pair-list 1 pair-list) "a")
  (check-equal? (pick-from-pair-list 2 pair-list) "c"))
