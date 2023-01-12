#lang racket
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

#; (define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
; improved version, further improved in 2.1
#; (define (make-rat n d)
    (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; 2.1
(define (make-rat n d)
  (let ((an (abs n))
    (ad (abs d))
    (sign (if (< (* n d) 0) -1 1)))
  (let ((g (gcd an ad)))
    (cons (* sign (/ an g)) (/ ad g)))))

(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))
(define test-rat (make-rat 4 -6))
(print-rat test-rat)

; 2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))
(define (x-point p)
    (car p))
(define (y-point p)
    (cdr p))
(define (make-segment p1 p2)
    (cons p1 p2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))
(define (average a b) (/ (+ a b) 2))
(define (midpoint-segment s) 
  (let (( x1 (x-point (start-segment s)))
    (y1 (y-point (start-segment s)))
    (x2 (x-point (end-segment s)))
    (y2 (y-point (end-segment s))))
    (make-point (average x1 x2) (average y1 y2))))

(define mysegment (make-segment (make-point 3.0 2) (make-point -8 -1.0)))
(print-point (midpoint-segment mysegment))

; 2.3
; Two representations
; segment and width
; length width rotation angle midpoint
; a way to get points

(define (rotate-point point angle)
    (define (deg2rad a) (/ (* pi a) 180))
    (let ((sine (sin (deg2rad angle)))
        (cosine (cos (deg2rad angle)))
        (x (x-point point))
        (y (y-point point)))
    (make-point (+ (* cosine x) (* -1 sine y))
        (+ (* sine x) (* cosine y)))))
(define p (make-point (/ 1 2) (/ (sqrt 3) 2)))
(print-point (rotate-point p 30))

(define (half a) (/ a 2))

(define (perimeter rect) 
    (* (+ (width rect) (height rect)) 2))
(define (area rect) 
    (* (width rect) (height rect)))
(define (square x) (* x x))
(define (distance p1 p2)
    (sqrt (+ 
        (square (- (x-point p1) (x-point p2)))
        (square (- (y-point p1) (y-point p2))))))
(define (seg-length segment)
    (distance (start-segment segment) (end-segment segment)))

; First representation
#|
(define (rectangle side width)
    (cons side width))
(define (side rect)
    (car rect))
(define (width rect)
    (cdr rect))
(define (height rect)
    (seg-length (side rect)))
    
(define r1 (rectangle (make-segment(make-point 1 2) 
    (make-point 4 6)) 7))
|#

; Second representation
(define (rectangle midpoint angle height width)
  (cons (cons midpoint angle) (cons height width)))

(define (width rect)
  (cdr (cdr rect)))
(define (height rect)
  (car (cdr rect)))

(define r1 (rectangle (make-point 3 2) 45 5 7))

(area r1)
(perimeter r1)

