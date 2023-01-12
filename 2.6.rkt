#lang racket

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;Define one and two directly (not in terms of zero and add-1) (Hint: Use substitution to evaluate (add-1 zero))
;(lambda (g) (lambda (y) y))
;(lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y)) f) x))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
; Give a direct definition of the addition procedure + (not in terms of repeated application of add-1).
(define (+ a b)
  (lambda (f) ((a f) ((b f) x))))

; My solutions agree with http://community.schemewiki.org/?sicp-ex-1.46
; Pretty sure https://sicp-solutions.net/post/sicp-solution-exercise-2-6/ has this one wrong,
; likely fooled by not renaming variables