#lang racket
(require rackunit "random-from.rkt")

; I'm not sure how best to test this. This is obviously inadequate.
(check member (random-from '(#\A #\B #\C #\1 #\2 #\3))
              '(#\A #\B #\C #\1 #\2 #\3))
(check member (random-from '(#\A #\B #\C #\1 #\2 #\3))
              '(#\A #\B #\C #\1 #\2 #\3))
(check member (random-from '(#\A #\B #\C #\1 #\2 #\3))
              '(#\A #\B #\C #\1 #\2 #\3))
