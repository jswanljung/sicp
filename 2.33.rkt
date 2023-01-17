#lang sicp

; Exercise 2.33.  Fill in the missing expressions to complete
; the following definitions of some basic list-manipulation
; operations as accumulations:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define l1 (list 1 2 3 4 5 6))
(define (square x) (* x x))
(define l2 (map square l1))

(length l2)
(append l1 l2)

; it works. It's kind of nuts how simple they were.