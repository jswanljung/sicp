#lang racket

; 2.24
; '(1 (2 (3 4)))
;
;
(define list1 '(1 3 (5 7) 9))
(car (cdr (car (cdr (cdr list1)))))

(define list2 '((7)))
(car (car list2))

(define list3 '(1 (2 (3 (4 (5 (6 7)))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3))))))))))))

; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)

(cons x y)

(list x y)