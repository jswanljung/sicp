#lang racket

; 2.17

(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))
   ))
(last-pair (list 23 72 149 34))

(define (reverse l)
  (define (reverse-iter reversed rest)
    (if (null? rest)
        reversed
        (reverse-iter (cons (car rest) reversed) (cdr rest))))
  (reverse-iter (list) l))
  
(reverse (list 1 4 9 16 25))

; 2.19

;(define us-coins (list 50 25 10 5 1))
(define us-coins (list 1 25 50 5 10)) ; order doesn't matter
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))

(cc 100 us-coins) ; should return 292


(define (even? i)
  (= (remainder i 2) 0))

; 2.20
(define (same-parity i . j)
  (define (same-parity? m n)
    (= (remainder (+ m n) 2) 0))
  (define (sp-listform i j)
    (cond ((null? j) (cons i '()))
    ((same-parity? i (car j))
        (cons i (sp-listform (car j) (cdr j))))
    (else
     (sp-listform i (cdr j)))))
  (sp-listform i j))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

(define nil null)