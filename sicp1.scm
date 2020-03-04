; Author: Eric Gullufsen
(define (square c) (* c c))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y) (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

; if |(guess / imroved_guess) - 1| < |guess / 100000|
(define (good-enough2? guess x)
  (< (abs (- (/ guess (improve guess x)) 1)) (abs (/ guess 100000))))

(define (sqrt-iter2 guess x)
  (if (good-enough2? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
