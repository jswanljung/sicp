#lang racket

(define (rproduct term a next b)
  (if (> a b)
      1
      (* (term a) (rproduct term (next a) next b))))

(define (iproduct term a next b)
  (define (iproduct-iter a result)
    (if (> a b)
        result
        (iproduct-iter (next a) (* (term a) result))))
  (iproduct-iter a 1))

(define product iproduct)

(define (inc x) (+ x 1))

(define (factorial x)
    (if (or (= x 0) (= x 1)) 1
        (product (lambda (x) x) 2 inc x)))

(define (square x) (* x x))

(define (pi-approx n)
  (* 4.0 2 (product (lambda (n) (square (/ (* 2 n) (- (* 2 n) 1)))) 2 inc (+ 2 n)) (/ 1 (- (* (+ 3 n) 2) 1))))


(define (smallest-divisor n)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (next test-divisor)))))
  (define (divides? i n)
    (= (remainder n i) 0))
  (find-divisor 2))

(define (next i)
  (if (= i 2)
      3
      (+ i 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
         (else (remainder (* base (expmod base (- exp 1) m)) m))))


(define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (random (- n 1))))



(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (cm-test n)
  (define (f-test a result)
    (if (or (not result) (= a n))
        result
        (f-test (+ a 1) (and result (= (expmod a n n) a)))))
  (f-test 2 #t))

(define (find-carmichael max)
  (define (test-next n)
    (if (and (cm-test n) (not (prime? n))) (displayln n) 0)
    (if (> n max) 0
          (test-next (next n))))
  (test-next 2))

(sqrt -1)

(define (solve-quadratic a b c)
  (define (+- x y)
    (list (+ x y) (- x y)))
  (+- (/ (* -1 b) (* 2 a) ) (/ (sqrt (- (square b) (* 4 a c))) (* 2 a))))



(define (expmod-mr base exp m)
  (define (square-with-test x)
    (after-square-test x (remainder (square x) m)))
  (define (after-square-test x y)
    (if (and (= y 1) (not (= x (- m 1))))
        0
        y))
  (cond ((= exp 0) 1)
        ((even? exp)
         (square-with-test (expmod-mr base (/ exp 2) m)))
         (else (remainder (* base (expmod-mr base (- exp 1) m)) m))))

(define (mr-prime? n times)
  (define (miller-rabin-test n)
    (define (expmod base exp m)
      (define (expmod-iter exp ntsqrt)
        (define (square-with-test x)
          (define (after-square-test x y)
            (if (= y 1) (not (= x (- m 1))))
                0
                y))
          (after-square-test x (remainder (square x) m)))
        (cond ((= exp 0) 1)
              ((even? exp)
               
    (define (next-a)
      (+ (random (- n 3)) 1))
    (define (try-it a)
      (= (expmod a (- n 1) n) 1))
    (try-it (next-a)))

  (cond ((= times 0) #t)
        ((miller-rabin-test n) (mr-prime? n (- times 1)))
        (else #f)))