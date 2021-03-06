(define (display-line x)
  (newline)
  (display x))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (stream-for-each-protected proc s lim)
  (if (< lim 1)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each-protected proc (stream-cdr s) (- lim 1)))))

(define (display-stream-limited s lim)
  (stream-for-each-protected display-line s lim))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

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

(define (pairs-correct s t) 
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (interleave 
     (stream-map (lambda (x) 
                  (list x (stream-car t)))
                 (stream-cdr s))
     (pairs-correct (stream-cdr s) (stream-cdr t))))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

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
           (if  (<= score-s1car score-s2car)
                (cons-stream 
                   s1car 
                   (merge-pairs-streams (stream-cdr s1) 
                                        s2
                                        wf))
                (cons-stream
                    s2car
                    (merge-pairs-streams s1 (stream-cdr s2) wf)))))))

(define (pairs-weighted s t w)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-pairs-streams
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs-weighted (stream-cdr s) (stream-cdr t) w)
    w)))

(define ans370 (pairs-weighted integers integers (lambda (x y) (+ x y))))

(define (sum-cubed x y)
    (+ (* x x x) (* y y y)))

(define (sum-squares x y)
  (+ (* x x) (* y y)))

(define ramaStream (pairs-weighted integers integers sum-cubed))

(define ramaStream2 (pairs-weighted integers integers sum-squares))

(define (ramanujan-numbers)
  (define (ramanujans all-sum-cubes)
    (let* ((current (stream-car all-sum-cubes))
           (next (stream-car (stream-cdr all-sum-cubes)))
           (ramanujan-candidate (sum-cubed (car current) (cadr current))))
      (cond ((= ramanujan-candidate
                (sum-cubed (car next) (cadr next)))
             (cons-stream (list ramanujan-candidate current next)
                          (ramanujans (stream-cdr (stream-cdr all-sum-cubes)))))
            (else (ramanujans (stream-cdr all-sum-cubes))))))
  (ramanujans ramaStream))

(define (ramanujan-trips)
  (define (ramanujan-trips-helper all-sum-cubes)
    (let* ((zero (stream-car all-sum-cubes))
           (one  (stream-car (stream-cdr all-sum-cubes)))
           (two  (stream-car (stream-cdr (stream-cdr all-sum-cubes))))
           (zero-zero (car zero))
           (zero-one  (cadr zero))
           (one-zero  (car one))
           (one-one   (cadr one))
           (two-zero  (car two))
           (two-one   (cadr two))
           (score-zero (sum-squares zero-zero zero-one))
           (score-one  (sum-squares one-zero one-one))
           (score-two  (sum-squares two-zero two-one)))
      (if (= score-zero score-one score-two)
          (cons-stream 
           (list score-zero zero one two)
           (ramanujan-trips-helper (stream-cdr all-sum-cubes)))
          (ramanujan-trips-helper (stream-cdr all-sum-cubes)))))
  (ramanujan-trips-helper ramaStream2))

(define (ans371 n) (display-stream-limited (ramanujan-numbers) n))
