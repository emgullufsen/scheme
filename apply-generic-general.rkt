#lang racket
(require "table-e.rkt")

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (coerce-em type-tags args)
    (if (empty? type-tags) (error "No method for these types (coerce-em)")
    (let ((the-type (type-tag (ca)))))
    )
    
)

(define (apply-generic-general op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coercion-procs (map (lambda (in-type) (if (same-type?))) type-tags))))))))


          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2) 
                  (error "No method for these types ()" (list op type-tags))
                  (let ((t1->t2 
                       (get-coercion type1
                                     type2))
                      (t2->t1 
                       (get-coercion type2 
                                     type1)))
                  (cond (t1->t2
                         (apply-generic 
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic 
                          op a1 (t2->t1 a2)))
                        (else
                         (error 
                          "No method for these types (*)"
                          (list 
                           op 
                           type-tags)))))))
              (error 
               "No method for these types (**)"
               (list op type-tags)))))))