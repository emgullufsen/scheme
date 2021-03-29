;; Exercise 4.35

(define (require p)
  (if (not p) (amb)))

(define (distinct? l)
  (if (null? l)
      true
      (let ((f (car l)) (s (cdr l)))
	    (if (null? s)
		true
		(let ((fs (car s)))
		      (if (= f fs)
			  false
			  (distinct? s)))))))

(define (an-integer-between l h)
 (require (<= l h))
 (amb l (an-integer-between (+ l 1) h)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) 
                    (* k k)))
        (list i j k)))))
