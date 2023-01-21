#lang sicp
(define (equal? alist anotherlist)
  (if (eq? alist anotherlist) true
        (and (eq? (car alist) (car anotherlist))
             (equal? (cdr alist) (cdr anotherlist)))))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))