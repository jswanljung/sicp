#lang racket
(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

#; (define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

#; (define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


; Exercise 2.7.  Alyssa's program is incomplete because she has
; not specified the implementation of the interval abstraction.
; Here is a definition of the interval constructor:

(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))

; Define selectors upper-bound and lower-bound to complete the
; implementation.
(define (lower-bound interval)
  (car interval) )

(define (upper-bound interval)
  (cdr interval))

; Exercise 2.8.  Using reasoning analogous to Alyssa's, describe
; how the difference of two intervals may be computed. Define a
; corresponding subtraction procedure, called sub-interval.

; supposing the bounds are all positive and that x > y
; we find the largest interval as followsss
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Exercise 2.9.  The width of an interval is half of the difference
; between its upper and lower bounds. The width is a measure of the
; uncertainty of the number specified by the interval. For some
; arithmetic operations the width of the result of combining two intervals
; is a function only of the widths of the argument intervals, whereas for
; others the width of the combination is not a function of the widths of
; the argument intervals. Show that the width of the sum (or difference)
; of two intervals is a function only of the widths of the intervals being
; added (or subtracted). Give examples to show that this is not true for
; multiplication or division.

(define (interval-width interval)
  (- (upper-bound interval) (lower-bound interval)))

(define x (make-interval 3.1 3.9))
(define y (make-interval 5.1 6.7))
(define z (make-interval 9.1 10.7))
(define (display-interval i)
  (display (lower-bound i))
  (display " < x < ")
  (displayln (upper-bound i)))
(displayln (interval-width x))
(displayln (interval-width y))
(displayln (interval-width z))
#|
(newline)
(displayln "Interval widths when adding or subtracting intervals (same widths)")
(displayln (interval-width (add-interval x y)))
(displayln (interval-width (add-interval x z)))
(displayln (interval-width (sub-interval y x)))
(displayln (interval-width (sub-interval z x)))
(displayln "Interval widths when multiplying or dividing intervals (same widths)")
(displayln (interval-width (mul-interval x y)))
(displayln (interval-width (mul-interval x z)))
(displayln (interval-width (div-interval x y)))
(displayln (interval-width (div-interval x z))) |#

; Exercise 2.10.  Ben Bitdiddle, an expert systems programmer,
; looks over Alyssa's shoulder and comments that it is not clear 
; what it means to divide by an interval that spans zero. Modify
;Alyssa's code to check for this condition and to signal an error
; if it occurs.

(define (spans-zero? y)
  (and (< (lower-bound y) 0) (> (upper-bound y) 0)))
(define (div-interval x y)
  (if (spans-zero? y)
      (error (quote div-interval) "Division by interval that spans zero!")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define zero-interval (make-interval -2.0 1.7))
(display-interval zero-interval)
; (display-interval (div-interval x zero-interval))

; 2.11
(define (mul-interval x y)
  (let ((ux (upper-bound x))
        (lx (lower-bound x))
        (uy (upper-bound y))
        (ly (lower-bound y)))
    (cond ((> lx 0) ; positive interval x
           (cond ((> ly 0) ; positive y
                  (make-interval (* lx ly) (* ux uy)))
                 ((< uy 0) ; negative y
                  (make-interval (* lx uy) (* ux ly)))
                 (else; y spans zero
                  (make-interval (* ly ux) (* uy ux)))))
          ((< ux 0) ; negative interval x
           (cond ((> ly 0) ; positive y
                  (make-interval (* lx uy) (* ux ly)))
                 ((< uy 0) ; negative y
                  (make-interval (* ux uy) (* lx ly)))
                 (else; y spans zero
                  (make-interval (* lx uy) (* ly lx)))))
          (else ; x spans zero
           (cond ((> ly 0) ; positive y
                  (make-interval (* lx uy) (* ux uy)))
                 ((< uy 0) ; negative y
                  (make-interval (* ux ly) (* lx ly)))
                 (else; both span zero
                  (make-interval (min (* ly ux) (* lx uy)) (max (* lx ly) (* uy ux)))))))))
(display-interval (mul-interval zero-interval x))
(display-interval (mul-interval x zero-interval))
(define xminus (make-interval (* (lower-bound x) -1) (* (upper-bound x) -1)))
(display-interval xminus)
(display-interval (mul-interval xminus y))
(display-interval (mul-interval y xminus))
(display-interval (mul-interval x y))
(display-interval (mul-interval zero-interval (make-interval -3.0 1.0)))
(display-interval zero-interval)


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; 2.12
(define (make-center-percent c p)
  (let ((dc (* c p 0.01)))
    (make-interval (- c dc) (+ c dc))))

(define (percent i)
  (* (/ (width i) (center i)) 100))

(display-interval (make-center-percent 100 5))
(percent x)
(display-interval x)