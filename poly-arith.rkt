#lang racket
(require "table-e.rkt")
(require "general-arithmetic.rkt")

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) 
  (null? term-list))
(define (make-term order coeff) 
  (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (add-terms L1 L2) (apply-generic 'add-terms L1 L2))
(define (mul-terms L1 L2) (apply-generic 'mul-terms L1 L2))
;; dense system
;; polys are lists of coefficients here
(define (install-dense-package)
    (define (adjoin-term-dense term term-list)
        (if (=zero? (coeff term))
            term-list
            (cons (coeff term) term-list)))
    (define (add-terms-dense L1 L2)
        (cond ((empty-termlist? L1) L2)
              ((empty-termlist? L2) L1)
              (else
                (let ((t1 (first-term L1)) 
                      (t2 (first-term L2))
                      (len1 (length L1))
                      (len2 (length L2)))
                        (let ((order1 (- len1 1)) (order2 (- len2 1)))
                            (cond 
                                ((> order1 order2) 
                                    (adjoin-term-dense t1 (add-terms (rest-terms L1) L2)))
                                ((< order1 order2) 
                                    (adjoin-term-dense t2 (add-terms L1 (rest-terms L2))))
                                (else              
                                    (adjoin-term-dense
                                        (make-term 
                                            (order1)
                                            (add t1 t2))
                                        (add-terms 
                                            (rest-terms L1)
                                            (rest-terms L2))))))))))
    (define (mul-terms-dense L1 L2) 
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms-dense (mul-term-by-all-terms-dense (make-term (- (length L1) 1) (first-term L1)) L2) (mul-terms-dense (rest-terms L1) L2))))
    (define (mul-term-by-all-terms-dense t1 L) 
        (if (empty-termlist? L)
            (the-empty-termlist)
            (let ((t2 (make-term (- (length L) 1) (first-term L))))
                (adjoin-term-dense
                    (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
                    (mul-term-by-all-terms-dense t1 (rest-terms L))))))
    (define (tag x) (attach-tag 'dense x))
    (put 'add-terms '(dense dense) (lambda (L1 L2) (tag (add-terms-dense L1 L2))))
    (put 'mul-terms '(dense dense) (lambda (L1 L2) (tag (mul-terms-dense L1 L2))))
    (put 'make 'dense (lambda (l) (tag l)))
    'done-dense
)

(install-dense-package)

(define (install-sparse-package)
    (define (adjoin-term-sparse term term-list)
        (if (=zero? (coeff term))
            term-list
            (cons term term-list)))
    (define (mul-terms-sparse L1 L2)
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms-sparse 
                (mul-term-by-all-terms-sparse (first-term L1) L2)
                (mul-terms-sparse (rest-terms L1) L2))))

    (define (mul-term-by-all-terms-sparse t1 L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (let ((t2 (first-term L)))
                (adjoin-term-sparse
                    (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
                    (mul-term-by-all-terms-sparse t1 (rest-terms L))))))

    (define (add-terms-sparse L1 L2)
        (cond ((empty-termlist? L1) L2)
              ((empty-termlist? L2) L1)
              (else
                (let ((t1 (first-term L1)) 
                      (t2 (first-term L2)))
                    (cond ((> (order t1) (order t2))
                            (adjoin-term-sparse t1 (add-terms-sparse (rest-terms L1) L2)))
                          ((< (order t1) (order t2))
                            (adjoin-term-sparse t2 (add-terms-sparse L1 (rest-terms L2))))
                          (else
                            (adjoin-term-sparse
                                (make-term (order t1) (add (coeff t1) (coeff t2)))
                                (add-terms-sparse (rest-terms L1) (rest-terms L2)))))))))
    (define (tag x) (attach-tag 'sparse x))
    (put 'add-terms '(sparse sparse) (lambda (L1 L2) (tag (add-terms-sparse L1 L2))))
    (put 'mul-terms '(sparse sparse) (lambda (L1 L2) (tag (mul-terms-sparse L1 L2))))
    (put 'make 'sparse (lambda (l) (tag l)))
    'done-sparse
)

(install-sparse-package)

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
     (variable? v2)
     (eq? v1 v2)))
  (define (add-poly p1 p2)
  (if (same-variable? (variable p1) 
                      (variable p2))
      (make-poly 
       (variable p1)
       (add-terms (term-list p1)
                  (term-list p2)))
      (error "Polys not in same var: 
              ADD-POLY"
             (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) 
                      (variable p2))
          (make-poly 
            (variable p1)
            (mul-terms (term-list p1) (term-list p2)))
          (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  ;(define (zero-poly-from-poly p) (make-poly (variable p) (list (make-term 0 -1))))
  ;(define (negation-poly-from-poly p) (mul-poly (zero-poly-from-poly p) p))
  ;; interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  ;(put 'sub '(polynomial polynomial)
       ;(lambda (p1 p2) (tag (add-poly p1 (negation-poly-from-poly p2)))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'make-sparse 'polynomial
       (lambda (var terms) 
         (tag (make-poly-sparse var terms))))
  (put 'make-dense 'polynomial
       (lambda (var terms)
         (tag (make-poly-dense var terms))))
  ;(put 'eqzero '(polynomial) (lambda (p) (or (empty-termlist? (term-list p)) (loop-eq? 0 (map coeff (term-list p))))))
  'done-poly
)
(install-polynomial-package)
(define (make-polynomial-sparse var terms)
  ((get 'make-sparse 'polynomial) var terms))

(define (make-polynomial-dense var terms) 
    ((get 'make-dense 'polynomial) var terms))

(define ex-poly 
    (make-polynomial-sparse
        'x 
        (list (make-term 3 6) (make-term 2 4) (make-term 1 -2) (make-term 0 -1))))