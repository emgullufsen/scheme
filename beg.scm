(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; this is linear recursive
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
		 (+ counter 1)
		 max-count)))

; this is linear iterative
(define (factorial2 n)
  (fact-iter 1 1 n))
