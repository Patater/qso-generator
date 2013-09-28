#lang racket
(require rackunit "qso-generator.rkt")

(check-equal? (strip-emptiness '("hi")) '("hi"))
(check-equal? (strip-emptiness '("hi" "guys")) '("hi" "guys"))
(check-equal? (strip-emptiness '("" "guys")) '("guys"))
(check-equal? (strip-emptiness '("hi" "")) '("hi"))
(check-equal? (strip-emptiness '("hi" "" ())) '("hi"))
(check-equal? (strip-emptiness '("hi" "" () "guys" ())) '("hi" "guys"))