#lang scheme
(define (make-accumulator initial-amount)
    (lambda (n) 
        (begin (set! initial-amount (+ initial-amount n)) initial-amount)))