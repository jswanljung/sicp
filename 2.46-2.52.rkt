#lang racket
(require (prefix-in r: racket/draw))
(require racket/gui)

(define *target* '())
(define *current-dc* '())  
(define image-size 300)

(define (*new-image*)
  (define target (r:make-bitmap image-size image-size)) 
  (define dc (new r:bitmap-dc% [bitmap target]))
  (send dc scale image-size image-size)
  (send dc set-pen "black" 0 'solid)
  (set! *current-dc* dc)
  (set! *target* target))


(define (draw-line start end)
    (send *current-dc* draw-line
         (xcor-vect start)
         (ycor-vect start)
         (xcor-vect end)
         (ycor-vect end)))
#;(define (drawme)
  (define me (scale 0.5 (bitmap "johan.jpg")))
  (*new-image* (make-frame (make-vect 0 150)
                           (make-vect 150 0)
                           (make-vect 150 0)))
  (set! *current-image*
        me)
  *current-image*)

(define (paint-in-frame painter frame)
    (*new-image*)
    (painter frame)
    (make-object image-snip% *target*))

(define (paint painter)
    (paint-in-frame
        painter
        (make-frame
            (make-vect 0 1)
            (make-vect 1 0)
            (make-vect 0 -1))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect vec)
  (car vec))
(define (ycor-vect vec)
  (cdr vec))

(define (add-vect u v)
  (make-vect (+ (xcor-vect u) (xcor-vect v))
             (+ (ycor-vect u) (ycor-vect v))))

(define (scale-vect scale u)
  (make-vect (* scale (xcor-vect u)) (* scale (ycor-vect u))))

(define (sub-vect u v)
  (add-vect u (scale-vect -1 v)))

; LIST VERSION WITH SELECTORS
#;(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
#;(define (origin-frame frame)
  (car frame))
#;(define (edge1-frame frame)
  (car (cdr frame)))
#;(define (edge2-frame frame)
  (car (cdr (cdr frame))))

; NESTED-PAIR VERSION
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame) ; only difference is here 
  (cddr frame))
  

(define myframe (make-frame (make-vect 1 2) (make-vect 3 2) (make-vect 2 -3)))
(origin-frame myframe)
(edge1-frame myframe)
(edge2-frame myframe)

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (bitmap->painter filename)
  (define source (r:read-bitmap filename))
  (define xscale (/ 1 (send source get-width)))
  (define yscale (/ 1 (send source get-height)))
  (lambda (frame)
    (let ((itransform (send *current-dc* get-transformation))
          (x0 (xcor-vect (origin-frame frame)))
          (y0 (ycor-vect (origin-frame frame)))
          (edge1x (xcor-vect (edge1-frame frame)))
          (edge1y (ycor-vect (edge1-frame frame)))
          (edge2x (xcor-vect (edge2-frame frame)))
          (edge2y (ycor-vect (edge2-frame frame))))
    
    (send *current-dc* transform (vector edge1x edge2x edge1y (* -1 edge2y) x0 y0))
      (send *current-dc* scale xscale yscale)
    (send *current-dc* draw-bitmap source 0 0)
    (send *current-dc* set-transformation itransform))))
(define drawme (bitmap->painter "johan.jpg"))

(paint drawme)


;2.48
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

; 2.49a
(define outline (segments->painter
                 (list
                  (make-segment (make-vect 0.0 0.0)
                                (make-vect 1.0 0.0))
                  (make-segment (make-vect 1.0 0.0)
                                (make-vect 1.0 1.0))
                  (make-segment (make-vect 1.0 1.0)
                                (make-vect 0.0 1.0))
                  (make-segment (make-vect 0.0 1.0)
                                (make-vect 0.0 0.0)))))
(paint outline)
; b
(define x (segments->painter
           (list
            (make-segment (make-vect 0.0 0.0)
                          (make-vect 1.0 1.0))
            (make-segment (make-vect 0.0 1.0)
                          (make-vect 1.0 0.0)))))

(paint x)

;c
(define diamond (segments->painter
                 (list
                  (make-segment (make-vect 0.5 0)
                                (make-vect 1 0.5))
                  (make-segment (make-vect 1 0.5)
                                (make-vect 0.5 1))
                  (make-segment (make-vect 0.5 1)
                                (make-vect 0 0.5))
                  (make-segment (make-vect 0 0.5)
                                (make-vect 0.5 0)))))

(paint diamond)

(define (points->segments sequence)
  (if (null? (cdr sequence))
      '()
      (let ((startx (caar sequence))
            (starty (cadar sequence))
            (endx (caadr sequence))
            (endy (cadr (cadr sequence))))
        (cons (make-segment (make-vect startx starty)
                            (make-vect endx endy))
              (points->segments (cdr sequence))))))

(define wavepoints (append ; ugly, but not uglier than the original.
                    (points->segments
                     '((0 0.83) (0.15 0.6) (0.32 0.65) (0.39 0.65) (0.34 0.83) (0.39 1)))
                    (points->segments
                     '((0 0.65) (0.15 0.4) (0.28 0.55) (0.33 0.47) (0.25 0)))
                    (points->segments
                     '((0.61 1) (0.66 0.83) (0.61 0.65) (0.76 0.65) (1 0.33)))
                    (points->segments
                     '((0.4 0) (0.5 0.3) (0.6 0)))
                    (points->segments
                     '((0.75 0) (0.65 0.47) (1 0.2)))))


(define wave (segments->painter wavepoints))
(paint wave)
;(paint-in-frame x (make-frame (make-vect 0 0) (make-vect 100.0 0.0) (make-vect 100.0 100.0))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(paint (shrink-to-upper-right wave))
(paint (flip-vert wave))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(paint (rotate90 wave))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(paint (squash-inwards wave))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(paint (beside wave wave))
;2.50
(define (flip-horiz painter)
    (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(paint (flip-horiz wave))

(define (rotate-180 painter)
    (transform-painter painter
                     (make-vect 1.0 1.0)   ; new origin
                     (make-vect 0.0 1.0)   ; new end of edge1
                     (make-vect 1.0 0.0))) ; new end of edge2

(paint (rotate-180 wave))

(define (rotate-270 painter)
    (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2
(paint (rotate-270 wave))

;2.51

 (define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0)
                              split-point))
          (paint-above
           (transform-painter painter2
                              split-point
                              (add-vect split-point (make-vect 1.0 0.0))
                              (make-vect 0 1.0))))
      (lambda (frame)
        (paint-above frame)
        (paint-below frame)))))

; version 2
(define (below2 painter1 painter2)
  (rotate90 (beside
             (rotate-270 painter1)
             (rotate-270 painter2))))
(paint (below wave x))
(paint (below2 wave x))

(paint (below drawme drawme))
