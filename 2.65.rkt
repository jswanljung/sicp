#lang sicp
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

#;(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

#;(define (adjoin-set x set)
  (if (null? set) (list x)
      (let ((y (car set)))
        (cond ((= x y) set)
              ((> x y)
               (cons y (adjoin-set x (cdr set))))
              (else (cons x set))))))

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x (car set1))
                    (y (car set2)))
                (cond ((= x y)
                       (cons x (union-set (cdr set1)
                                          (cdr set2))))
                      ((< x y)
                       (cons x (union-set (cdr set1) set2)))
                      (else (cons y (union-set set1 (cdr set2)))))))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (intersection-tree tree1 tree2)
  (list->tree (intersection-set (tree->list tree1) (tree->list tree2))))
(define (union-tree tree1 tree2)
  (list->tree (union-set (tree->list tree1) (tree->list tree2))))


(define l1 (list->tree (list 1 4 6 10 14 17 22 26 31 33 39)))
(define l2 (list->tree (list 13 15 17 19 31 32 17)))
(intersection-tree l1 l2)
(union-tree l1 l2)