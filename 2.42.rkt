#lang sicp

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (queens board-size)
  (define empty-board nil)
  (define (adjoin-position row col positions)
    (cons (list col row) positions))
  (define (safe? k positions)
    (if (< k 2)
        true
        (let ((kth (car (cdr (car positions))))
              (adjacent (car (cdr (car (cdr positions))))))
          (and (< 1 (abs (- kth adjacent)))
          (accumulate (lambda (pos rest) (and  (not (= kth (car (cdr pos)))) rest))
                      true
                      (cdr positions))))))
    
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))
(length (queens 8))
(queens 5)