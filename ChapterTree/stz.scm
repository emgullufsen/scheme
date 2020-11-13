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
