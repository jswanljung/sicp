#lang racket

(define (average a b)
  (/ (+ a b) 2))

(define (square x) (* x x))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)
#;(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

#;(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

;(sqrt 2)

(define (m x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

;(cube-root 100)

;(expt 100 (/ 1 3))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

(define (cube x) (* x x x))
((deriv cube) 5)


(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

#;(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(sqrt 2)

; 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 2 -3 2) 1)

; 1.41
(define (double g)
  (lambda (x) (g (g x))))
(define (inc n) (+ n 1))

(((double (double double)) inc) 5)


; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)


; 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)


; 1.44

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smoothed f n)
  ((repeated smooth n) f))


; 1.45
(define (nth-root x n)
  (let ((damp-times (if (< n 2) ; This is unnecessary, n < 2 makes no sense
    1
    (floor (log n 2))))) ; seems to increase every time we reach power of 2
  (fixed-point 
  ((repeated average-damp damp-times)
  (lambda (y) (/ x (expt y (- n 1)))))
  1.0)))

(nth-root 2.0 2)
(nth-root 27.0 3)
(nth-root 16 4)
(nth-root 34 5)
(nth-root 83 8)

; 1.46
; This happens to be nearly identical to the "most improved"
; version at http://community.schemewiki.org/?sicp-ex-1.46
(define (iterative-improve good-enough? improve)
  (define (ii guess) (if (good-enough? guess) 
    guess
    (ii (improve guess))))
    ii)

(define (sqrt x) 
  ((iterative-improve 
    (lambda (guess) (< (abs (- (square guess) x)) 0.001))
    (lambda (guess) (average guess (/ x guess)))) 1.0))

(sqrt 100)


; Here we have to calculate the next value twice. I see no way around
; this without modifying iterative-improve, which is what they do
; here: https://sicp-solutions.net/post/sicp-solution-exercise-1-46/
; http://community.schemewiki.org/?sicp-ex-1.46 does it like I did
(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess) (< (abs (guess - (f guess) tolerance))))))
    f)