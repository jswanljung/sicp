#lang racket

; recursive version
#; (define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

; iterative version
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (inc s) (+ s 1))

(define (identity x) x)

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (and (not (= n 1))
  (= n (smallest-divisor n))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (filtered-accumulate combiner null-value term a next b predicate)
    (define (iter a result)
      (define (filter s)
        (if (predicate s)
            s
            null-value))
      (if (> a b)
          result
          (iter (next a) (combiner result (term (filter a))))))
  (iter a null-value))

;1.33 a
(filtered-accumulate + 0 square 0 inc 10 prime?)

;1.33 b
(define (relprimeproduct n)
  (define (relativeprime? x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 identity 1 inc n relativeprime?))

(relprimeproduct 10)