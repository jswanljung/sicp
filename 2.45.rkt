#lang sicp
(#%require sicp-pict)
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (split op1 op2)
  (define (splitter painter n)
      (if (= n 0)
      painter
      (let ((smaller (splitter painter (- n 1))))
        (op1 painter (op2 smaller smaller)))))
  splitter)

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


(paint (flipped-pairs einstein))
(paint (right-split einstein 5))
(paint (corner-split einstein 6))