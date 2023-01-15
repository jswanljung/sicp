#lang racket

; 2.21
(define (square x) (* x x))
(define nil null)
#; (define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))
(define (square-list items)
  (map square items))

(square-list (list 1 2 3 4))
; Should give (1 4 9 16)

; 2.23

(define (foreach proc items)
  (if (null? items)
      #t
      (begin
        (proc (car items))
        (foreach proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))