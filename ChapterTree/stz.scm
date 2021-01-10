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

(define invints (stream-map (lambda (i) (/ 1 i)) integers))

;; exercise 3.59 part a.
(define (integrate-series s) (mul-streams s invints))

;; part b.
(define exp-series (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (stream-map (lambda (h) (* h -1)) sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

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
     (scale-stream (stream-cdr s1) (stream-car s2))
     (scale-stream (stream-cdr s2) (stream-car s1)))
    (cons-stream
     0
     (mul-series (stream-cdr s1) (stream-cdr s2))))))

;; exercise 3.61
(define (invert-unit-series S)
  (cons-stream 1 (mul-series (scale-stream -1 (stream-cdr S)) (invert-unit-series S))))

;; exercise 3.62
(define (div-series S1 S2)
  (if (= (stream-car S2) 0)
    (error "denom const term == zero")
    (mul-series 
      S1 
      (invert-unit-series 
        (scale-stream 
          (/ 1 (stream-car S2)) 
          S2)))))
