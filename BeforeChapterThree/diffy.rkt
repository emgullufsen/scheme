#lang racket
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)
(provide deriv)

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! 
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr! 
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (variable? exp) (symbol? exp))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (=number? a num) (and (number? a) (= a num)))

(define (make-sum s1 s2) 
  (cond ((=number? s1 0) s2)
        ((=number? s2 0) s1)
        ((and (number? s1) (number? s2)) (+ s1 s2))
        (else (list '+ s1 s2))
  )
)

(define (make-product p1 p2) 
  (cond ((or (=number? p1 0) (=number? p2 0)) 0)
        ((=number? p1 1) p2)
        ((=number? p2 1) p1)
        ((and (number? p1) (number? p2)) (* p1 p2))
        (else (list '* p1 p2))
  )
)

(define (make-exponent base power) 
  (cond ((=number? power 0) 1)
        ((=number? power 1) base)
        (else (list '** base power))
  )
)

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) 
            (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (operator exp)) (operands exp) var))
    )
)

(define (install-sum-procedures)
  (define (deriv-sum exp var)
    (make-sum (deriv (car exp) var) (deriv (cadr exp) var))
  )
  (put 'deriv '+ deriv-sum)
)

(define (install-product-procedures)
  (define (deriv-product exp var)
    (make-sum (make-product (deriv (car exp) var) (cadr exp)) (make-product (car exp) (deriv (cadr exp) var)))
  )
  (put 'deriv '* deriv-product)
)

(define (install-exponent-procedures)
  (define (deriv-exponent exp var) 
    (let ((base (car exp)) (power (cadr exp)))
      (make-product (make-product power (make-exponent base (- power 1))) (deriv base var))
    )
  )
  (put 'deriv '** deriv-exponent)
)

(install-sum-procedures)
(install-product-procedures)
(install-exponent-procedures)