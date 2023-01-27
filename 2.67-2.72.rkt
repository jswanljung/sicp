#lang sicp
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; 2.67
(decode sample-message sample-tree)

; 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

#;(define (encode-symbol symbol tree)
  (define (check-branch branch)
    (if (leaf? branch) (if (eq? symbol (symbol-leaf branch))
                              'match
                              'no-match)
           (let ((l-result (check-branch (left-branch branch))))
             (cond ((eq? l-result 'no-match)
                    (let ((r-result (check-branch (right-branch branch))))
                      (cond ((eq? r-result 'no-match) 'no-match)
                            ((eq? r-result 'match) '(1))
                            (else (cons 1 r-result)))))
                   ((eq? l-result 'match) '(0))
                   (else (cons 0 l-result))))))
  (let ((tree-result (check-branch tree)))
    (if (eq? tree-result 'no-match)
        (error "no match for symbol -- ENCODE-SYMBOL" symbol)
        tree-result)))

; My solution works, but I should have checked the symbols lists for the branches,
; I forgot they existed. Using that makes the code considerably simpler, see
; examples here: http://community.schemewiki.org/?sicp-ex-2.68

; New attempt

#;(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((memq symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "no match for symbol -- ENCODE SYMBOL" symbol))))
         
(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
         ((memq symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        ((memq symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))

        (else (error "no match for symbol -- ENCODE SYMBOL" symbol))))
         
(encode '(A D A B B C A) sample-tree)               

; 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge branches)
  (if (null? (cdr branches))
      (car branches)
      (successive-merge (adjoin-set (make-code-tree (car branches) (cadr branches)) (cddr branches)))))
; There are lots of solutions here: http://community.schemewiki.org/?sicp-ex-2.69. The shortest (which
; I believe are correct) are nearly identical to mine.

 (define test-tree (generate-huffman-tree '((A 3) (B 5) (C 6) (D 6)))) 
  
 (encode '(A B C D) test-tree)

; 2.70
(define lyrics-tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3)
                                                   (GET 2) (YIP 9) (JOB 2) (WAH 1))))
(define song '(GET A JOB
                   SHA NA NA NA NA NA NA NA NA
                   GET A JOB
                   SHA NA NA NA NA NA NA NA NA
                   WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                   SHA BOOM))

(define encoded-song (encode song lyrics-tree))
(length encoded-song)
(* (length song) 3)

(define (make-list n)
  (define (iter i result)
    (if (= i n) result
      (iter (+ i 1) (cons (- n i) result))))
  (iter 0 '()))

(define (random-symbol i)
  (define (random-char)
  (integer->char (+ (random 26) 64)))
  (string->symbol (apply string (map (lambda (x) (random-char)) (make-list i)))))

(define (random-leaves symbol-size n)
  (let ((nlist (make-list n)))
    (let ((symbols (map (lambda (x) (random-symbol symbol-size)) nlist)))
      (map (lambda (x y) (list x (expt 2 (- y 1)))) symbols nlist))))

(define (timed-encode symbol-number iterations)
  (define (last list)
    (cond ((null? list) '())
          ((null? (cdr list)) (car list))
          (else (last (cdr list)))))
  (let ((myleaves (random-leaves 10 symbol-number)))
    (let ((big-tree (generate-huffman-tree myleaves))
          (firstleaf (car myleaves))
          (lastleaf (last myleaves)))
  (define (timed-encode start-time symbol)
    (define (iter i)
      (if (> i iterations)
          (begin (display (- (runtime) start-time))
          (newline))
          (begin (encode-symbol symbol big-tree)
                 (iter (+ i 1)))))
    (iter 0))
  (timed-encode (runtime) (car firstleaf))
  (timed-encode (runtime) (car lastleaf)))))

(timed-encode 100 5000)
(timed-encode 200 5000)
(timed-encode 300 5000)
(timed-encode 400 5000)
(timed-encode 500 5000)
(timed-encode 600 5000)
(timed-encode 700 5000)

; The best case for highest frequency is O(1) but that depends on checking the right branch first,
; otherwise we end up doing memq on a growing symbol list, which is O(n).
; for the lowest frequency we have O(n).