(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))
 
(define factorials (cons-stream 1 (mul-streams factorials integers)))

(define (partial-sums s)
  (define ss (cons-stream (car s) (add-streams ss (stream-cdr s))))
  ss)

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter 
           pred
           (stream-cdr stream))))
        (else (stream-filter 
               pred 
               (stream-cdr stream)))))
(define x 
  (stream-map 
   show 
   (stream-enumerate-interval 0 10)))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream 
                   s1car 
                   (merge (stream-cdr s1) 
                          s2)))
                 ((> s1car s2car)
                  (cons-stream 
                   s2car 
                   (merge s1 
                          (stream-cdr s2))))
                 (else
                  (cons-stream 
                   s1car
                   (merge 
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))

(define (scale-stream s c)
  (stream-map (lambda (x) (* c x)) s))

;; exercise 3.56
(define S1 (cons-stream 1 (merge (scale-stream S1 2) (merge (scale-stream S1 3) (scale-stream S1 5)))))

;; exercise 3.60
(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1) (stream-car s2))
   (add-streams
    (add-streams
     (scale-stream s1 (stream-car s2))
     (scale-stream s2 (stream-car s1)))
    (cons-stream
     0
     (mul-series (stream-cdr s1) (stream-cdr s2))))))
