					; eric gullufsen
(define (sum-integers a b)
  (if (> a b) 
      0 
      (+ a (sum-integers (+ a 1) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (sum2 term a next b counter n)
  (cond ((= counter n) (term (g a b counter n)))
	((= counter 0) (+ (term (g a b counter n)) (sum2 term a next b (next counter) n)))
	((odd? counter) (+ (* 4 (term (g a b counter n))) (sum2 term a next b (next counter) n)))
	((even? counter) (+ (* 2 (term (g a b counter n))) (sum2 term a next b (next counter) n)))))

(define (g a b c n)
  (+ a (* c (/ (- b a) n))))

(define (inc x) (+ x 1))

(define (integral2 f a b n) (* (/ (/ (- b a) n) 3) (sum2 f a inc b 0 n)))
