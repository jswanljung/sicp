#lang racket
; 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define testpair (cons 2 4))
(cdr testpair)
(car testpair)