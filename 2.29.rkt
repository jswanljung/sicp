#lang racket
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight structure)
  (if (not (pair? structure))
      structure
      (+ (total-weight (branch-structure (left-branch structure)))
         (total-weight (branch-structure (right-branch structure))))))

(define (balanced? structure)
  (define (torque branch)
    (* (branch-length branch) (total-weight (branch-structure branch))))
  (if (not (pair? structure)) #t
      (let ((left (left-branch structure))
            (right (right-branch structure)))
        (and (balanced? (branch-structure (left-branch structure)))
             (balanced? (branch-structure (right-branch structure)))
             (= (torque left) (torque right))))))
    

(define balanced-r-branch (make-branch 4 100))
(define unbalanced-r-branch (make-branch 4 10))
(define balanced-l-branch (make-branch 8 50))
(define 2-balanced (make-mobile balanced-l-branch balanced-r-branch))
(define 2-unbalanced (make-mobile balanced-l-branch unbalanced-r-branch))


(define m1
  (make-mobile (make-branch 2 3)
               (make-branch 2 3)))
(balanced? m1) ; yes
(define m2
  (make-mobile (make-branch 3 6)
               (make-branch 3 m1)))
(balanced? m2) ;yes

(define m3
    (make-mobile (make-branch 3 6)
                 (make-branch 1.5 m2)))
(balanced? m3) ;yes

(define m4
    (make-mobile (make-branch 3 m2)
                 (make-branch 3 m3)))
(balanced? m4) ; nope

(define m5
    (make-mobile (make-branch 3 m2)
                 (make-branch 2 m3)))
(balanced? m5) ; yes
