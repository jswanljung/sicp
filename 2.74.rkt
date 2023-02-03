#lang racket

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (division-tag personnel-file)
  ; returns the unique symbol for that file, used to lookup
  ; selectors/procedures specific to that division
  )
(define (records personnel-file)
  ;returns the records in the file without the tag)
(define (division employee-record)
  (car employee-record)); Assuming tag is just added as first element in list.
(define (data employee-record)
  (cadr employee-record))
(define (tag-record data division)
  (list division data))
(define (record-in-file? employee-name personnel-file)
  (let ((divtag (division-tag personnel-file))) ; could have done without let
    ((get record-in-file? divtag) employee-name (records personnel-file))))
(define (get-record employee-name personnel-file)
  (cond ((not (record-in-file? employee-name personnel-file))
		(error "record not in file! -- GET-RECORD" employee-name))
	(else (let ((divtag division-tag personnel-file))
	 (tag-record 
		((get get-record divtag) 
		 employee-name (records personnel-file))
		divtag))))) 
; I see how this would work, but its super-boring and a
; nothing you can run and test without implementing lots of stuff. Orka.
