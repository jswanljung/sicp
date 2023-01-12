#lang racket
; non-negative integer b
(define (power a b)
  (if (= b 0) 1
      (* a (power a (- b 1)))))

; find the number of times b divides z
(define (factors z b)
  (if (= (remainder z b) 0)
         (+ 1 (factors (/ z b) b))
         0))
            
(define (cons a b)
  (* (power 2 a) (power 3 b)))

(define (car z)
  (factors z 2))
(define (cdr z)
  (factors z 3))

(define testpair (cons 0 5))
(car testpair)
(cdr testpair)