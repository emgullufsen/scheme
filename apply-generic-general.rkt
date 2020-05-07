#lang racket
(require "table-e.rkt")

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (loop-eq? e lst) 
  (if (empty? lst) 
    '#t
    (if (eq? e (car lst)) (loop-eq? e (cdr lst)) '#f)))

(define (coerce-em type-tags args)
    (if (empty? type-tags) (error "No method for these types (coerce-em)")
    (let ((the-type (car type-tags)))
      (let ((args-convert 
              (map 
                (lambda (a) 
                  (let ((coerce-proc (get-coercion (type-tag a) the-type)))
                    (if (eq? the-type (type-tag a)) 
                      a
                      (if coerce-proc (coerce-proc a) a))))
                args)))
        (if (loop-eq? the-type (map type-tag args-convert))
          args-convert
          (coerce-em (cdr type-tags) args))))))

(define (apply-generic-general op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (apply-generic-general op . (coerce-em type-tags args))))))