#lang racket
(define *op-table* (make-hash))
(define *coercion-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (put-coercion type1 type2 proc)
  (hash-set! *coercion-table* (list type1 type2) proc))

(define (get-coercion type1 type2)
  (hash-ref *coercion-table* (list type1 type2) '()))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number) contents (cons type-tag contents)))
(define (type-tag datum)
  (cond
    [(number? datum) 'scheme-number]
    [(pair? datum) (car datum)]
    [else (error "Bad tagged datum -- TYPE-TAG" datum)]))
(define (contents datum)
  (cond
    [(number? datum) datum]
    [(pair? datum) (cdr datum)]
    [else (error "Bad tagged datum -- CONTENTS" datum)]))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z)
    (car z))
  (define (imag-part z)
    (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z)
    (car z))
  (define (angle z)
    (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'polar x))

  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  ; reduce the type-tag list to remove duplicates
  ; check that it isn't just one element because then there's nothing to do.
  ; for each type check if the others can be coerced to it
  ;
  (define (unique ulist)
    (define (unique-iter from to)
      (if (null? from)
          to
          (unique-iter (cdr from) (if (memq (car from) to) to (append to (list (car from)))))))
    (unique-iter ulist '()))
  (define (find-coercion-target types)
    (define (test-coercion type tlist)
      (cond
        [(null? tlist) #t]
        [(or (eq? type (car tlist)) (not (null? (get-coercion (car tlist) type))))
         (test-coercion type (cdr tlist))]
        [else #f]))
    (define (test-types tlist)
      (cond
        [(null? tlist) '()]
        [(test-coercion (car tlist) types) (car tlist)]
        [else (test-types (cdr tlist))]))
    (if (= (length types) 1) '() (test-types types)))
  (let ([type-tags (map type-tag args)])
    (define (no-match-error)
      (error "No method for these types" (list op type-tags)))
    (let ([proc (get op type-tags)])
      (if (not (null? proc))
          (apply proc (map contents args))
          (let ([cotype (find-coercion-target (unique type-tags))])
            (if (null? cotype)
                (no-match-error)
                (apply apply-generic
                       op
                       (map (lambda (type arg)
                              (if (eq? type cotype) arg ((get-coercion type cotype) arg)))
                            type-tags
                            args))))))))

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (add x y)
  (apply-generic 'add x y))
(define (sub x y)
  (apply-generic 'sub x y))
(define (mul x y)
  (apply-generic 'mul x y))
(define (div x y)
  (apply-generic 'div x y))
(define (equ? x y)
  (apply-generic 'equ? x y))
(define (=zero? x)
  (apply-generic '=zero? x))
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y)))
  (put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y))))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x)
    (car x))
  (define (denom x)
    (cdr x))
  (define (make-rat n d)
    (let ([g (gcd n d)]) (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x)
    (attach-tag 'rational x))
  (define (equ? x y)
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  (define (=zero? x)
    (= (numer x) 0))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (put-coercion 'rational 'scheme-number (lambda (n) (/ (numer n) (denom n))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z)
    (attach-tag 'complex z))
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (= (magnitude z) 0))
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  'done)
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (square x)
  (* x x))
(install-polar-package)
(install-rectangular-package)
(install-complex-package)
(install-rational-package)
(install-scheme-number-package)
(add (make-scheme-number 3) (make-scheme-number 4))
(define z (make-complex-from-real-imag 3 4))
(magnitude z)
; (magnitude z)
; (apply-generic 'magnitude z) gets the magnitude function
; defined in the complex package and calls it on the arg
; stripped of its outer tag. This just points back to the same
; function again, but now the tag is rectangular, so it
; gets the magnitude function from the rectangular function
; (defined internally) and calls it.
(equ? 3 4)
(equ? (make-rational 2 3) (make-rational 4 6))
(equ? (make-rectangular 2 3) (make-rectangular 2 3))
(equ? (make-rectangular 1 0) (make-polar 1 0))

(define (exp x y)
  (apply-generic 'exp x y))
(add 3 z)
