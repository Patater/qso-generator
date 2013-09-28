#lang racket
(require "random-from.rkt")
(require "pick.rkt")

(define (make-chain)
  (list '() (make-hash)))

(define (chain-hash chain)
  (car (cdr chain)))

(define (chain-start-transition-list chain)
  (car chain))

(define (add-start-n-gram-to-chain s chain)
  (list (update-transition-list s (chain-start-transition-list chain)) (chain-hash chain)))

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

(define (fetch-item-count-pair l)
  (car l))
(define (fetch-first-item l)
  (car (fetch-item-count-pair l)))
(define (fetch-rest-items l)
  (cdr l))
(define (fetch-first-count l)
  (car (cdr (fetch-item-count-pair l))))
(define (update-transition-list s l)
  (cond ((null? l) (list (list s 1)))
        ((equal? s (fetch-first-item l))
         (let ([new-pair (list s (+ 1 (fetch-first-count l)))])
           (cons new-pair (cdr l))))
        (#t (cons (car l) (update-transition-list s (cdr l))))))

(define (update-markov-chain-hash! n s chain position)
    (cond ((< position n) chain)
          (#t (let* ([n+1-gram (n-gram-at (+ n 1) position s)]
                     [key (take n+1-gram n)]
                     [next-word (last n+1-gram)]
                     [hash (chain-hash chain)])
                (cond ((hash-has-key? hash key)
                       (hash-set! hash key (update-transition-list next-word (hash-ref hash key))))
                      (#t (hash-set! hash key (update-transition-list next-word '()))))
                (update-markov-chain-hash! n s
                                           (cond ((= position n) (add-start-n-gram-to-chain key chain))
                                                 (#t chain))
                                           (- position 1))))))

(define (generate-markov-chain n s)
  (let ([position (- (length s) 1)] [chain (make-chain)])
    (update-markov-chain-hash! n s chain position)))

(define (build-word-level-markov-chain-from-file n path)
  (define (parse-line! chain)
  (let ([line (read-line)])
    (cond ((equal? line eof) chain)
          (#t (let* ([corpus (string-split line " ")] [position (- (length corpus) 1)])
                 (parse-line! (update-markov-chain-hash! n corpus chain position)))))))
  (with-input-from-file path
    (lambda ()
        (parse-line! (make-chain)))))

(define (build-letter-level-markov-chain-from-file n path)
  (define (parse-line! chain)
  (let ([line (read-line)])
    (cond ((equal? line eof) chain)
          (#t (let* ([corpus (string-split line "")] [position (- (length corpus) 1)])
                 (parse-line! (update-markov-chain-hash! n corpus chain position)))))))
  (with-input-from-file path
    (lambda ()
        (parse-line! (make-chain)))))

(define (traverse-markov-chain n-gram markov-chain word-limit)
  (cond ((zero? word-limit) '())
        ((hash-has-key? (chain-hash markov-chain) n-gram)
         (let ([next-word (pick (hash-ref (chain-hash markov-chain) n-gram))])
           (cons next-word
                 (traverse-markov-chain (append (cdr n-gram)
                                                (list next-word))
                                        markov-chain
                                        (- word-limit 1)))))
        (#t '())))

(define (start-markov-chain-traversal markov-chain word-limit)
  (let ([initial-n-gram (pick (chain-start-transition-list markov-chain))])
    (append initial-n-gram
          (traverse-markov-chain initial-n-gram
                                 markov-chain
                                 (- word-limit (length initial-n-gram))))))

(define (generate-similar-corpus n-gram-n corpus word-limit)
  (let ([markov-chain (generate-markov-chain n-gram-n corpus)])
    (start-markov-chain-traversal markov-chain word-limit)))


(define (build-hierarchical-markov-chain-from-file path)
  (list (build-word-level-markov-chain-from-file 2 path)
        (build-word-level-markov-chain-from-file 1 path)
        (build-letter-level-markov-chain-from-file 3 path)
        (build-letter-level-markov-chain-from-file 2 path)
        (build-letter-level-markov-chain-from-file 1 path)))

; We need to be able to fetch the transition list from the hierarchical markov
; chain at any level at any time given an hchain, a level (like
; hchain-letter-1), and history object of sorts, give transition list
; appropriate for the specified level.
(define (hchain-word-2 hchain)
  (car hchain))
(define (hchain-word-1 hchain)
  (car (cdr hchain)))
(define (hchain-letter-3 hchain)
  (car (cdr (cdr hchain))))
(define (hchain-letter-2 hchain)
  (car (cdr (cdr (cdr hchain)))))
(define (hchain-letter-1 hchain)
  (car (cdr (cdr (cdr (cdr hchain))))))

(define (string-is-composed-of-symbols? s symbols)
  (subset? (list->set (string->list (if (list? s) (string-join s "") s))) (list->set symbols)))

(define (filter-transition-list tlist symbols)
  (cond ((null? tlist) '())
        ((string-is-composed-of-symbols? (fetch-first-item tlist) symbols)
         (cons (fetch-item-count-pair tlist) (filter-transition-list (cdr tlist) symbols)))
        (#t (filter-transition-list (cdr tlist) symbols))))

(provide n-gram-at
         n-grams
         update-transition-list
         chain-hash
         generate-markov-chain
         build-word-level-markov-chain-from-file
         build-hierarchical-markov-chain-from-file
         chain-start-transition-list
         filter-transition-list
         hchain-word-2
         hchain-word-1
         hchain-letter-3
         hchain-letter-2
         hchain-letter-1
         generate-similar-corpus
         string-is-composed-of-symbols?)
