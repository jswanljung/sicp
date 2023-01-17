#lang sicp

; Exercise 2.35.  Redefine count-leaves from section 2.2.2 as an accumulation:

#; (define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate
   (lambda (x y) (+ x y))
   0
   (map (lambda (node)
          (if (not (pair? node))
                   1
                   (count-leaves node))) t)))

; My solution is very similar to the uppermost solution here:
; http://community.schemewiki.org/?sicp-ex-2.35
; There they point out that empty pairs are counted as leaves, which
; can easily be remedied.

(define x (cons (list 1 2) (list 3 4)))

(count-leaves x)

(count-leaves (list x x 4))
