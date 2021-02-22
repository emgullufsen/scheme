(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

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

(define (merge-pairs-streams s1 s2 wf)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (s1A   (car  s1car))
                (s1B   (cadr s1car))
                (s2a   (car  s2car))
                (s2b   (cadr s2car))
                (score-s1car (wf s1A s1B))
                (score-s2car (wf s2A s2B)))
           (cond ((< score-s1car score-s2car)
                  (cons-stream 
                   s1car 
                   (merge-pairs-streams (stream-cdr s1) 
                                        s2
                                        wf)))
                 ((> score-s1car score-s2car)
                  (cons-stream 
                   s2car 
                   (merge-pairs-streams s1 
                                        (stream-cdr s2)
                                        wf)))
                 (else
                  (cons-stream 
                   s1car
                   (merge-pairs-streams 
                    (stream-cdr s1)
                    (stream-cdr s2)
                    wf))))))))

(define (pairs-weighted s t wf)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-pairs-streams
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t))
    wf)))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define pairs-weighted-sum 
  (pairs-weighted 
   integers 
   integers 
   (lambda (a b)
     (+ a b))))

(define (b-filter-func pair)
  (let* ((a (car pair))
         (b (cadr pair))
         (a2 (remainder a 2))
         (a3 (remainder a 3))
         (a5 (remainder a 5))
         (b2 (remainder b 2))
         (b3 (remainder b 3))
         (b5 (remainder b 5)))
    (if (or (= 0 a2) (= 0 a3) (= 0 a5) (= 0 b2) (= 0 b3) (= 0 b5))
        #f
        #t)))

(define (b-weighting-func a b) (+ (* 2 a) (* 3 b) (* 5 a b)))

(define stream-b 
  (stream-filter 
   b-filter-func
   (pairs-weighted
    integers
    integers
    b-weighting-func)))
