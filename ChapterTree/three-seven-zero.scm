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

(define (pairs-c s t wf) 
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-pairs-streams
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (merge-pairs-streams 
     (stream-map (lambda (x) 
                  (list x (stream-car t)))
                 (stream-cdr s))
     (pairs-c (stream-cdr s) (stream-cdr t) wf)
     wf) 
    wf)))

(define rama-c 
  (pairs-c 
   integers 
   integers 
   (lambda (a b) (+ (* a a a) (* b b b)))))

(define rama-cc 
  (pairs-weighted 
   integers 
   integers 
   (lambda (a b) (+ (* a a a) (* b b b)))))

(define (stream-for-each-protected-2 proc s lim)
  (if (< lim 1)
      'done
      (begin 
        (proc (stream-car s) (stream-car (stream-cdr s)))
        (stream-for-each-protected-2 proc (stream-cdr s) (- lim 1)))))

(define (weigher a b)
  (let ((ca (car a)) (sa (cadr a)) (cb (car b)) (sb (cadr b)))
    (if (= (+ (* ca ca ca) (* sa sa sa)) (+ (* cb cb cb) (* sb sb sb)))
      #t
      #f)))
 
(define (go-rama-c z)
  (stream-for-each-protected-2 
   (lambda (a b)
     (if (weigher a b)
         (display-line a)))
   rama-c
   z))

(define (go-rama-cc z)
  (stream-for-each-protected-2 
   (lambda (a b)
     (if (weigher a b)
         (display-line a)))
   rama-cc
   z))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define pairs-weighted-sum 
  (pairs-weighted 
   integers 
   integers 
   (lambda (a b)
     (+ a b))))

(define rama 
  (pairs-weighted
   integers
   integers
   (lambda (a b)
     (+ (* a a a) (* b b b)))))

(define (merge-weighted-b s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (if (<= (weight s1car) (weight s2car))
               (cons-stream s1car 
                            (merge-weighted-b (stream-cdr s1) 
                                            s2 
                                            weight))
               (cons-stream s2car 
                            (merge-weighted-b s1 
                                            (stream-cdr s2) 
                                            weight)))))))
 
(define (weighted-pairs-b s t weight) 
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted-b
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs-b (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (ramujan-numbers)
  (define (sum-cubed x)
    (let ((i (car x)) (j (cadr x)))
      (+ (* i i i) (* j j j))))
  (define (ramujans all-sum-cubes)
    (let* ((current (stream-car all-sum-cubes))
           (next (stream-car (stream-cdr all-sum-cubes)))
           (ramujan-candidate (sum-cubed current)))
      (cond ((= ramujan-candidate
                (sum-cubed next))
             (cons-stream (list ramujan-candidate current next)
                          (ramujans (stream-cdr (stream-cdr all-sum-cubes)))))
            (else (ramujans (stream-cdr all-sum-cubes))))))
  (ramujans (weighted-pairs-b integers 
                            integers 
                            sum-cubed)))

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

