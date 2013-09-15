#lang racket
(require "random-from.rkt")
(require "pick.rkt")

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

(define (update-transition-list s l)
  (define (fetch-item-count-pair l)
    (car l))
  (define (fetch-first-item l)
    (car (fetch-item-count-pair l)))
  (define (fetch-first-count l)
    (car (cdr (fetch-item-count-pair l))))
  (cond ((null? l) (list (list s 1)))
        ((equal? s (fetch-first-item l))
         (let ([new-pair (list s (+ 1 (fetch-first-count l)))])
           (cons new-pair (cdr l))))
        (#t (cons (car l) (update-transition-list s (cdr l))))))

(define (update-markov-chain-hash! n s h position)
    (cond ((< position n) '())
          (#t (let* ([n+1-gram (n-gram-at (+ n 1) position s)]
                     [key (take n+1-gram n)]
                     [next-word (last n+1-gram)])
                (cond ((hash-has-key? h key)
                       (hash-set! h key (update-transition-list next-word (hash-ref h key))))
                      (#t (hash-set! h key (update-transition-list next-word '()))))
                (update-markov-chain-hash! n s h (- position 1))))))

(define (generate-markov-chain n s)
  (let ([position (- (length s) 1)] [h (make-hash)])
    (update-markov-chain-hash! n s h position)
    h))

(define (build-word-level-markov-chain-from-file n path)
  (define (parse-line! h)
  (let ([line (read-line)])
    (cond ((equal? line eof) '())
          (#t (let* ([corpus (string-split line " ")] [position (- (length corpus) 1)])
                 (update-markov-chain-hash! n corpus h position)
                  (parse-line! h)
                  h)))))
  (with-input-from-file path
    (lambda ()
        (parse-line! (make-hash)))))

(define (traverse-markov-chain n-gram markov-chain word-limit)
  (cond ((zero? word-limit) '())
        ((hash-has-key? markov-chain n-gram)
         (let ([next-word (pick (hash-ref markov-chain n-gram))])
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
         update-transition-list
         generate-markov-chain
         build-word-level-markov-chain-from-file
         generate-similar-corpus)
