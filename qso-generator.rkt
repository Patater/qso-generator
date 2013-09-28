#lang racket
(require srfi/14) ; char-set-contains
(require "markov.rkt")
(require "callsign-generate.rkt")
(require "pick.rkt")

; Since we just work with strings and not any lisp item, we need to define some
; uncommonly used characters to represent some prosigns.
(define BT# #\#)
(define SK@ #\@)
(define AR! #\!)
(define KN$ #\$)

; Two suggested Koch Method learning orders
(define MFJ-418-order
  (append
   '(#\W #\B #\M #\H #\A #\T #\J #\S #\N #\I
     #\O #\D #\E #\L #\K #\Z #\G #\C #\U #\Q
     #\R #\V #\F #\P #\Y #\X #\5 #\. #\7 #\/
     #\9 #\, #\1 #\6 #\8 #\? #\2 #\0 #\4 #\3)
    (list BT# SK@ AR! KN$)))

(define N1IRZ-order
  (append
   '(#\K #\M #\R #\S #\U #\A #\P #\T #\L #\O
     #\W #\I #\. #\N #\J #\E #\F #\0 #\Y #\V
     #\, #\G #\5 #\/ #\Q #\9 #\Z #\H #\3 #\8
     #\B #\? #\4 #\2 #\7 #\C #\1 #\D #\6 #\X)
    (list BT# SK@ AR! KN$)))

; We need to know previous n-gram for currently doing layer.
(define (traverse-layer tlist fallback label take-it-n fall-back-n)
  (if (null? tlist) (fallback)
      (pick (list (list (list label (pick tlist)) take-it-n)
                  (list (fallback) fall-back-n)))))
(define (emit-type xsymbol)
  (car xsymbol))
(define (emit-symbol xsymbol)
  (car (cdr xsymbol)))

(define (generate-random-text hierarchical-markov-chain
                              mastered-symbols
                              symbol-limit)
  (let ([word-2-chain (hchain-word-2 hierarchical-markov-chain)]
        [word-1-chain (hchain-word-1 hierarchical-markov-chain)]
        [letter-3-chain (hchain-letter-3 hierarchical-markov-chain)]
        [letter-2-chain (hchain-letter-2 hierarchical-markov-chain)]
        [letter-1-chain (hchain-letter-1 hierarchical-markov-chain)])
    ; We need to filter the transition list before picking from it.
    (let ([filtered-word-2-tlist
           (filter-transition-list (chain-start-transition-list word-2-chain)
                                   mastered-symbols)]
          [filtered-word-1-tlist
           (filter-transition-list (chain-start-transition-list word-1-chain)
                                   mastered-symbols)]
          [filtered-letter-3-tlist
           (filter-transition-list (chain-start-transition-list letter-3-chain)
                                   mastered-symbols)]
          [filtered-letter-2-tlist
           (filter-transition-list (chain-start-transition-list letter-2-chain)
                                   mastered-symbols)]
          [filtered-letter-1-tlist
           (filter-transition-list (chain-start-transition-list letter-1-chain)
                                   mastered-symbols)])
    (define (traverse-word-2) (traverse-layer filtered-word-2-tlist
                                              traverse-word-1
                                              "word-2"
                                              8 2))
    (define (traverse-word-1) (traverse-layer filtered-word-1-tlist
                                              traverse-letter-3
                                              "word-1"
                                              8 2))
    (define (traverse-letter-3) (traverse-layer filtered-letter-3-tlist
                                                traverse-letter-2
                                                "letter-3"
                                                8 2))
    (define (traverse-letter-2) (traverse-layer filtered-letter-2-tlist
                                                traverse-letter-1
                                                "letter-2"
                                                8 2))
    (define (traverse-letter-1) (traverse-layer filtered-letter-1-tlist
                                                traverse-random
                                                "letter-1" 8 2))
    (define (traverse-random) (list "random"
                                    (list->string
                                     (list (random-from mastered-symbols)))))
    (define (generate-initial-word-history n-gram)
      (list n-gram
            (if (>= (length n-gram) 1) (take-right n-gram 1) n-gram)
            (let ([letter-list
                   (drop (drop-right
                          (string-split (string-join n-gram) "") 1) 1)])
              (if (>= (length letter-list) 3)
                  (take-right letter-list 3) letter-list))
            (let ([letter-list
                   (drop (drop-right
                          (string-split (string-join n-gram) "") 1) 1)])
              (if (>= (length letter-list) 2)
                  (take-right letter-list 2) letter-list))
            (let ([letter-list
                   (drop (drop-right
                          (string-split (string-join n-gram) "") 1) 1)])
              (if (>= (length letter-list) 1)
                  (take-right letter-list 1) letter-list))))
    (define (generate-initial-letter-history n-gram)
      (list (let ([word-list (string-split (string-join n-gram "") " ")])
              (if (>= (length word-list) 2)
                  (take-right word-list 2) word-list))
            (let ([word-list (string-split (string-join n-gram "") " ")])
              (if (>= (length word-list) 1)
                  (take-right word-list 1) word-list))
            n-gram
            (if (>= (length n-gram) 2) (take-right n-gram 2) n-gram)
            (if (>= (length n-gram) 1) (take-right n-gram 1) n-gram)))
    (let* ([xinitial-n-gram (traverse-word-2)]
           [initial-n-gram-type (emit-type xinitial-n-gram)]
           [initial-n-gram (emit-symbol xinitial-n-gram)]
           [initial-symbol
            (cond ((string=? initial-n-gram-type "word-2")
                   (string-join initial-n-gram " "))
                  ((string=? initial-n-gram-type "word-1")
                   (string-join initial-n-gram " "))
                  ((string=? initial-n-gram-type "letter-3")
                   (string-join initial-n-gram ""))
                  ((string=? initial-n-gram-type "letter-2")
                   (string-join initial-n-gram ""))
                  ((string=? initial-n-gram-type "letter-1")
                   initial-n-gram )
                  ((string=? initial-n-gram-type "random")
                   initial-n-gram))]
           [initial-history
            (cond ((string=? initial-n-gram-type "word-2")
                   (generate-initial-word-history initial-n-gram))
                  ((string=? initial-n-gram-type "word-1")
                   (generate-initial-word-history initial-n-gram))
                  ((string=? initial-n-gram-type "letter-3")
                   (generate-initial-letter-history initial-n-gram))
                  ((string=? initial-n-gram-type "letter-2")
                   (generate-initial-letter-history initial-n-gram))
                  ((string=? initial-n-gram-type "letter-1")
                   (generate-initial-letter-history initial-n-gram))
                  ((string=? initial-n-gram-type "random")
                   (generate-initial-letter-history initial-n-gram)))])
      (string-append initial-symbol
                     (traverse-hchain hierarchical-markov-chain
                                      initial-history
                                      mastered-symbols
                                      (- symbol-limit 1)))))))
(define (traverse-hchain hierarchical-markov-chain
                         history
                         mastered-symbols
                         symbol-limit)
  (let ([word-2-hash
         (chain-hash (hchain-word-2 hierarchical-markov-chain))]
        [word-1-hash
         (chain-hash (hchain-word-1 hierarchical-markov-chain))]
        [letter-3-hash
         (chain-hash (hchain-letter-3 hierarchical-markov-chain))]
        [letter-2-hash
         (chain-hash (hchain-letter-2 hierarchical-markov-chain))]
        [letter-1-hash
         (chain-hash (hchain-letter-1 hierarchical-markov-chain))])
    (let ([filtered-word-2-tlist
           (filter-transition-list
            (hash-ref word-2-hash (word-2-history history) '())
            mastered-symbols)]
          [filtered-word-1-tlist
           (filter-transition-list
            (hash-ref word-1-hash (word-1-history history) '())
            mastered-symbols)]
          [filtered-letter-3-tlist
           (filter-transition-list
            (hash-ref letter-3-hash (letter-3-history history) '())
            mastered-symbols)]
          [filtered-letter-2-tlist
           (filter-transition-list
            (hash-ref letter-2-hash (letter-2-history history) '())
            mastered-symbols)]
          [filtered-letter-1-tlist
           (filter-transition-list
            (hash-ref letter-1-hash (letter-1-history history) '())
            mastered-symbols)])
      (define (traverse-word-2)
        (traverse-layer filtered-word-2-tlist
                        traverse-word-1 "word-2" 8 2))
      (define (traverse-word-1)
        (traverse-layer filtered-word-1-tlist
                        traverse-letter-3 "word-1" 8 2))
      (define (traverse-letter-3)
        (traverse-layer filtered-letter-3-tlist
                        traverse-letter-2 "letter-3" 8 2))
      (define (traverse-letter-2)
        (traverse-layer filtered-letter-2-tlist
                        traverse-letter-1 "letter-2" 8 2))
      (define (traverse-letter-1)
        (traverse-layer filtered-letter-1-tlist
                        traverse-random "letter-1" 8 2))
      (define (traverse-random)
        (list "random" (list->string (list (random-from mastered-symbols)))))
    (define (generate-word-history history word)
      (list (cond ((equal? (length (word-2-history history)) 2)
                   (let ([word-2-list
                          (string-split (string-join
                                         (append (word-2-history history)
                                                 (list word)))
                                        " ")])
                     (if (>= (length word-2-list) 2)
                         (take-right word-2-list 2) word-2-list)))
                  ((equal? (length (word-2-history history)) 1)
                   (append (word-2-history history) (list word)))
                  (#t (list 'A word)))
            (cond ((equal? (length (word-1-history history)) 1)
                   (let ([word-1-list
                          (string-split (string-join
                                         (append (word-1-history history)
                                                 (list word)))
                                        " ")])
                     (if (>= (length word-1-list) 1)
                         (take-right word-1-list 1) word-1-list)))
                  (#t (list 'A word)))
            (let ([letter-3-list
                   (drop (drop-right
                          (string-split (string-join
                                         (list (string-join
                                                (letter-3-history history) "")
                                               word)
                                         " ") "") 1) 1)])
              (if (>= (length letter-3-list) 3)
                  (take-right letter-3-list 3) letter-3-list))
            (let ([letter-2-list
                   (drop (drop-right
                          (string-split (string-join
                                         (list (string-join
                                                (letter-2-history history) "")
                                               word)
                                         " ") "") 1) 1)])
              (if (>= (length letter-2-list) 2)
                  (take-right letter-2-list 2) letter-2-list))
            (let ([letter-1-list
                   (drop (drop-right
                          (string-split (string-join
                                         (list (string-join
                                                (letter-1-history history) "")
                                               word)
                                         " ") "") 1) 1)])
              (if (>= (length letter-1-list) 1)
                  (take-right letter-1-list 1) letter-1-list))))
    (define (generate-letter-history history letter)
      (list (cond ((string=? " " letter)
                   (cdr (append (word-2-history history) (list ""))))
                  (#t (let ([word-2-list
                             (string-split (string-join
                                            (list (string-join
                                                   (word-2-history history))
                                                  letter)
                                            "") " ")])
                        (if (>= (length word-2-list) 2)
                            (take-right word-2-list 2) word-2-list))))
            (cond ((string=? " " letter)
                   (cdr (append (word-1-history history) (list ""))))
                  (#t (let ([word-1-list
                             (string-split (string-join
                                            (list (string-join
                                                   (word-1-history history))
                                                  letter)
                                            "") " ")])
                        (if (>= (length word-1-list) 1)
                            (take-right word-1-list 1) word-1-list))))
            (let ([letter-3-list
                   (drop (drop-right
                          (string-split (string-join
                                         (list (string-join
                                                (letter-3-history history)
                                                "")
                                               letter)
                                         "") "") 1) 1)])
              (if (>= (length letter-3-list) 3)
                  (take-right letter-3-list 3) letter-3-list))
            (let ([letter-2-list
                   (drop (drop-right
                          (string-split (string-join
                                         (list (string-join
                                                (letter-2-history history)
                                                "")
                                               letter)
                                         "") "") 1) 1)])
              (if (>= (length letter-2-list) 2)
                  (take-right letter-2-list 2) letter-2-list))
            (let ([letter-1-list
                   (drop (drop-right
                          (string-split (string-join
                                         (list (string-join
                                                (letter-1-history history)
                                                "") letter)
                                         "") "") 1) 1)])
              (if (>= (length letter-1-list) 1)
                  (take-right letter-1-list 1) letter-1-list))))
      (let ([next-xsymbol
             (cond ((zero? symbol-limit) '())
                   ((hash-has-key? word-2-hash (word-2-history history))
                    (traverse-word-2))
                   ((hash-has-key? word-1-hash (word-1-history history))
                    (traverse-word-1))
                   ((hash-has-key? letter-3-hash (letter-3-history history))
                    (traverse-letter-3))
                   ((hash-has-key? letter-2-hash (letter-2-history history))
                    (traverse-letter-2))
                   ((hash-has-key? letter-1-hash (letter-1-history history))
                    (traverse-letter-1))
                   (#t (traverse-random)))])
        (if (null? next-xsymbol) ""
            (let ([next-symbol (emit-symbol next-xsymbol)]
                  [next-type (emit-type next-xsymbol)])
              (cond ((string=? next-symbol "") "")
                    (#t (let (; Based on the type of n-gram generated,
                              ; construct all of the possible previous n-grams
                              ; for use in traversing the chain.
                              [future-history
                               (cond ((string=? next-type "word-2")
                                      (generate-word-history history
                                                             next-symbol))
                                     ((string=? next-type "word-1")
                                      (generate-word-history history
                                                             next-symbol))
                                     ((string=? next-type "letter-3")
                                      (generate-letter-history history
                                                               next-symbol))
                                     ((string=? next-type "letter-2")
                                      (generate-letter-history history
                                                               next-symbol))
                                     ((string=? next-type "letter-1")
                                      (generate-letter-history history
                                                               next-symbol))
                                     ((string=? next-type "random")
                                      (generate-letter-history history
                                                               next-symbol)))])
                          (string-append
                           (cond ((string=? next-type "word-2") " ")
                                 ((string=? next-type "word-1") " ")
                                 (#t ""))
                           next-symbol
                           (traverse-hchain hierarchical-markov-chain
                                            future-history
                                            mastered-symbols
                                            (- symbol-limit 1))))))))))))

; We store n-gram history in the following format
; '(("My" "name") ("My") ("" "M" "y") ("" "M") (""))
; P.S. I suck at lisp data structures.
(define (word-2-history history)
  (car history))
(define (word-1-history history)
  (car (cdr history)))
(define (letter-3-history history)
  (car (cdr (cdr history))))
(define (letter-2-history history)
  (car (cdr (cdr (cdr history)))))
(define (letter-1-history history)
  (car (cdr (cdr (cdr (cdr history))))))

(define (generate-de mastered-symbols)
  (string-join (list (if (member #\D mastered-symbols) "D" "")
                     (if (member #\E mastered-symbols) "E" "")) ""))

(define (generate-qso-ending callsign-to callsign-from mastered-symbols)
  (define (first-style-ending)
    (list (if (member AR! mastered-symbols) "AR!" "")
          callsign-to (generate-de mastered-symbols) callsign-from
          (let ([first-ending-pick
                 (list (if (member #\K mastered-symbols) '("K" 29) '())
                       (if (member KN$ mastered-symbols) '("KN$" 123) '())
                       (if (member SK@ mastered-symbols) '("SK@" 50) '()))])
            (pick first-ending-pick))))
  (define (second-style-ending)
    (list callsign-to (generate-de mastered-symbols) callsign-from
          (let ([second-ending-pick
                 (list (if (member AR! mastered-symbols) '("AR!" 1) '())
                       (if (member SK@ mastered-symbols) '("SK@" 1) '()))])
            (pick second-ending-pick))))
  ; TODO If we only have AR, we should always do second ending, right? In most
  ; of our learning orders, we learn SK before AR, so this is a non-issue
  ; practically.
  (pick (list (list (first-style-ending) 1)
              (list (second-style-ending) 1))))

(define (strip-emptiness s)
  (cond ((null? s) '())
        ((null? (car s)) (strip-emptiness (cdr s)))
        ((and (string? (car s)) (string=? "" (car s)))
         (strip-emptiness (cdr s)))
        (#t (cons (car s) (strip-emptiness (cdr s))))))

(define (generate-random-qso mastered-symbols)
  (let ([callsign-to
         (list->string (generate-random-callsign mastered-symbols))]
        [callsign-from
         (list->string (generate-random-callsign mastered-symbols))]
        [markov-chain (build-hierarchical-markov-chain-from-file "qso.txt")])
    (string-join
     (strip-emptiness
      (append (list (if (member #\V mastered-symbols) "VVV VVV" "")
                    callsign-to (generate-de mastered-symbols) callsign-from)
              (list (generate-random-text markov-chain
                                          (cons #\space mastered-symbols) 80))
              (let ([ending (generate-qso-ending callsign-to
                                                 callsign-from
                                                 mastered-symbols)])
                (if (null? ending) "" ending)))))))

(provide strip-emptiness
         generate-random-qso
         generate-random-text)
