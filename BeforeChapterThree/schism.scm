(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
	 n)
	((= (remainder n test-divisor) 0)
	 test-divisor)
	(else (find-divisor
	       n
	       (+ test-divisor 1)))))
