#lang racket
; Complete the following definition of a procedure that generates
; the set of subsets of a set and give a clear explanation of why
; it works:
(define nil null)
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset)
                            (cons (car s) subset)) rest)))))

; the set of all subsets is the set of subsets without the first element
; union each of those subsets but including the first element
; This sets things up for easy recursion

(subsets (list 1 2 3))