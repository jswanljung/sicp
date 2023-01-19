#lang racket

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (mr-prime? n tries)
  (define (next-a n)
    (+ 2 (random (- n 3))))
  (define (mr-test a n)
    (define (squaremod x)
      (remainder (square x) n))
    (define (squaremod-with-test x)
      (define (test s) (if (and (= s 1) (not (or (= x 1) (= x (- n 1)))))
                     0
                     s))
      (test (squaremod x)))
    (define (expmod exp reached-d)
      (cond ((= exp 0) 1)
      ((even? exp)
       ; not entirely clear if we need to stop testing once we reach an odd exponent?
       ; Pretty sure that we can, but does it matter?
          ((if reached-d squaremod squaremod-with-test) 
           (expmod (/ exp 2) reached-d))) 
      (else (remainder (* a (expmod (- exp 1) #t)) n))))
    (= (expmod (- n 1) #f) 1))
  (if (= tries 0)
         #t
        (and (mr-test (next-a n) n) (mr-prime? n (- tries 1)))))
                     
  (define (find-prime x max tries)
    (cond ((> x max) #t)
        (else (if (mr-prime? x tries) (displayln x) (display ""))
        (find-prime (+ x 1) max tries))))