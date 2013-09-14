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

(define (update-markov-chain-hash! n s h position)
    (cond ((< position n) '())
          (#t (let* ([n+1-gram (n-gram-at (+ n 1) position s)]
                     [key (take n+1-gram n)]
                     [next-word (last n+1-gram)])
                (cond ((hash-has-key? h key)
                       (hash-set! h key (cons next-word (hash-ref h key))))
                      (#t (hash-set! h key (list next-word))))
                (update-markov-chain-hash! n s h (- position 1))))))

(define (generate-markov-chain n s)
  (let ([position (- (length s) 1)] [h (make-hash)])
    (update-markov-chain-hash! n s h position)
    h))

(define (traverse-markov-chain n-gram markov-chain word-limit)
  (cond ((zero? word-limit) '())
        ((hash-has-key? markov-chain n-gram)
         (let ([next-word (random-from (hash-ref markov-chain n-gram))])
           (cons next-word
                 (traverse-markov-chain (append (cdr n-gram)
                                                (list next-word))
                                        markov-chain
                                        (- word-limit 1)))))
        (#t '())))

(define (generate-similar-corpus n-gram-n corpus word-limit)
  (let ([first-n-gram (n-gram-at n-gram-n (- n-gram-n 1) corpus)]
        [markov-chain (generate-markov-chain n-gram-n corpus)])
    (append first-n-gram
            (traverse-markov-chain first-n-gram
                                   markov-chain
                                   (- word-limit n-gram-n)))))

(provide n-gram-at
         n-grams
         generate-markov-chain
         generate-similar-corpus)
