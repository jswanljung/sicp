#lang racket
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define phi (/ (+ 1 (sqrt 5)) 2.))
((lambda (x) (+ 1 (/ 1 x))) phi)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.)

(define (fixed-point2 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (displayln next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point2 (lambda (x) (/ (log 1000) (log x))) 4)

(fixed-point2 (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 4)

; recursive
#;(define (cont-frac n d k)
  (define (shift-index f) (lambda (i) (f (+ i 1))))
  (/ (n 1) (+ (d 1) (if (= k 0) 0 (cont-frac (shift-index n) (shift-index d) (- k 1))))))

; iterative
(define (cont-frac n d k)
  (define (cont-iter i result)
    (if (= i 0)
        result
    (cont-iter (- i 1)
          (/ (n i) (+ (d i) result)))))
  (cont-iter (+ 1 k) 0))

(define (reciprocal-phi k)
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k))

(displayln "***")
(define (kphi k)
  (define (iter i)
    (if (> i k) 
        (displayln "done")
        (begin (displayln (reciprocal-phi i))
        (iter (+ i 1)))))
  (iter 1))

  (kphi 11)

(define (ed i)
  (if (= (remainder (+ i 1) 3) 0)
         (* 2 (/ (+ i 1) 3))
         1))

  (define (e k)
    (+ 2 (cont-frac (lambda (i) 1.0) ed k)))

  (e 10)

(define (square x) (* x x))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1) x (* -1 (square x))))
  (define (d i)
    (- (* 2 i) 1))
  (displayln (n 2))
  (cont-frac n d k))

(tan-cf (/ pi 3) 5)