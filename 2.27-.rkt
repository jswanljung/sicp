#lang racket

(define test-list (list 1 2 3 4 5 6))

(define (reverse l)
  (define (reverse-iter reversed rest)
    (if (null? rest)
        reversed
        (reverse-iter (cons (car rest) reversed) (cdr rest))))
  (reverse-iter (list) l))

(reverse test-list)

(define x (list (list 1 2) (list 3 4)))

(displayln x)
(reverse x)
;((3 4) (1 2))

(define (deep-reverse l)
    (define (reverse-iter reversed rest)
    (if (null? rest)
        reversed
        (let ((crest (car rest)))
          (reverse-iter (cons (if (pair? crest) (deep-reverse crest) crest) reversed) (cdr rest)))))
  (reverse-iter (list) l))

(deep-reverse x)

; 2.28
(define nil null)
(define (fringe tree)
  (if (null? tree) nil
      (let ((branch (car tree))
            (rest (fringe (cdr tree))))
        (if (pair? branch)
            (append (fringe branch) rest)
            (cons branch rest)))))

(fringe x)
(fringe (list x x))