;; eg
;; sicp 3.3.3 - 2D tables
;; exercise 3.25 done baby!
(define (assoc key records)
  	(cond ((null? records) false)
	      ((equal? key (caar records)) (car records))
	      (else (assoc key (cdr records)))))

(define (make-subtable keys value)
  (if (null? (cdr keys))
      (cons (car keys) value)
      (list (car keys) (make-subtable (cdr keys) value))))

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup keys)
	(let lookup-helper ((k keys) (t local-table))
	  (let ((rec (assoc (car k) (cdr t))))
	    (if rec
		(if (null? (cdr k))
		    (cdr rec)
		    (lookup-helper (cdr k) rec))
		false))))
    
    (define (insert! keys value)
	(let insert!-helper ((k keys) (t local-table))
	  (let ((rec (assoc (car k) (cdr t))))
	    (if rec
		(if (null? (cdr k))
		    (set-cdr! rec value)
		    (insert!-helper (cdr k) rec))
		(set-cdr! local-table (cons (make-subtable keys value) (cdr local-table)))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
