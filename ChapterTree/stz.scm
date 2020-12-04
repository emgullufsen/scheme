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
