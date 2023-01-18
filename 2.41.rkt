#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; Exercise 2.41.  Write a procedure to find all ordered triples of
; distinct positive integers i, j, and k less than or equal to a given
; integer n that sum to a given integer s.


(define (all-ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval (+ j 1) n)))
                    (enumerate-interval (+ i 1) n)))
           (enumerate-interval 1 n)))

(all-ordered-triples 6)

(define (triples-with-sum s n)
  (define (check-sum? triple)
    (= (accumulate + 0 triple) s))
  (filter check-sum? (all-ordered-triples n)))

(triples-with-sum 10 12)
