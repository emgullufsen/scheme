#lang scheme
(define (count-pairs-bad x)
    (if (not (pair? x))
        0
        (+ (count-pairs-bad (car x)) (count-pairs-bad (cdr x)) 1)
    )
)
;; 3.17
(define count-pairs
  (let ((seen null))
    (lambda (x)
      (cond ((not (pair? x)) 0)
            ((memq x seen) 0)
            (else (set! seen (cons x seen))
                  (+ (count-pairs (car x))
                     (count-pairs (cdr x))
                     1))))))
;; 3.18
(define (has-cycle? xs)
  (define seen null)
  (define (cycle-aux ys)
    (cond ((null? ys) #f)
          ((memq (car ys) seen) #t)
          (else (set! seen (cons (car ys) seen))
                (cycle-aux (cdr ys)))))
  (cycle-aux xs))
 