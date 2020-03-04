;;eric gullufsen
;;march zero-three, two-thousand twenty

;;exercise 1.11 - SICP

;; recursive solution
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

;; iterative solution
(define (f-iter fn1 fn2 fn3 counter)
  (if (= counter 0)
      fn3
      (f-iter (p fn1 fn2 fn3) fn1 fn2 (- counter 1))))

(define (p fn1 fn2 fn3)
  (+ fn1 (* 2 fn2) (* 3 fn3)))	
      
(define (f2 n) (f-iter 2 1 0 n))
