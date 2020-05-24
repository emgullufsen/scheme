#lang racket
(require "table-e.rkt")

(provide type-tag exp apply-generic make-rational make-scheme-number 
         make-complex-from-real-imag make-integer raise drop project 
         make-real attach-tag =zero? add sub mul div loop-eq? contents)

(define (square x) ((get 'mul (map type-tag (list x x))) (contents x) (contents x)))

(define (attach-tag type-tag contents)
    (if (eq? type-tag 'scheme-number)
        contents
        (cons type-tag contents)
    )
)

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (loop-eq? e lst) 
  (if (empty? lst) 
    '#t
    (if (eq? e (car lst)) (loop-eq? e (cdr lst)) '#f)))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: 
              CONTENTS" datum))))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (same-type? o1 o2) (eq? (type-tag o1) (type-tag o2)))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (square-root (apply-generic 'add (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (arctangent (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (apply-generic 'mul r (cosine a)) (apply-generic 'mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (apply-generic 'mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (apply-generic 'mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (square-root (+ (square x) (square y)))
          (arctangent y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)

(define (gcd a b)
    (if (= b 0) 
        a 
        (gcd b (remainder a b))))

; the generic operations that act on regular numbers,
; rationals, and complex numbers
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equal x y))
(define (=zero? x) (apply-generic 'eqzero x))
; for exercise 2.81 - only defined & installed for scheme-numbers 
(define (exp x y) 
  (apply-generic 'exp x y))
(define (square-root x) (apply-generic 'sqrt x))
(define (arctangent x y) (apply-generic 'atan x y))
(define (cosine a) (apply-generic 'cos a))
(define (sine a) (apply-generic 'sin a))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
(define (tower-loc? x)
  (let ((taggy (type-tag x)))
    (cond ((or (eq? taggy 'scheme-number) (eq? taggy 'integer)) 0)
          ((eq? taggy 'rational)   1)
          ((eq? taggy 'real)       2)
          ((eq? taggy 'complex)    3)
          (else (error "bad type tag: tower-loc?")))))
(define (higher-type? x y) 
  (let ((tl-x (tower-loc? x)) (tl-y (tower-loc? y)))
    (if (> tl-x tl-y) x y)))
(define (highest-in-list list)
  (define (helper holder lst) 
    (if (empty? lst) 
      holder
      (helper (higher-type? holder (car lst)) (cdr lst))))
  (helper (car list) (cdr list))) 

(define (raise-em argslist)
  (if (loop-eq? (type-tag (highest-in-list argslist)) (map type-tag argslist)) 
    argslist 
    (raise-em (map 
                (lambda (a) 
                  (if (> (tower-loc? (highest-in-list argslist)) (tower-loc? a)) 
                    (raise a) 
                    a)) 
                argslist))))

(define (apply-generic op . args)
  (let ((proc (get op (map type-tag args))))
    (if proc 
      (apply proc (map contents args))
      (let ((dropped-args (raise-em (map drop args))))
        (let ((proc2 (get op (map type-tag dropped-args))))
          (if proc2
            (apply proc2 (map contents dropped-args))
            (error "none fo dese doggy...")))))))
  
; regular (scheme) numbers
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equal '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'eqzero '(scheme-number) (lambda (x) (= 0 x)))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) 
           (tag (expt x y))))
  (put 'sqrt '(scheme-number) (lambda (x) (sqrt x)))
  (put 'atan '(scheme-number scheme-number) (lambda (x y) (atan x y)))
  (put 'sin '(scheme-number) (lambda (x) (sin x)))
  (put 'cos '(scheme-number) (lambda (x) (cos x)))
  (put 'raise '(scheme-number) (lambda (x) (make-rational x 1)))
  'done)

(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'equal '(integer integer)
       (lambda (x y) (= x y)))
  (put 'make 'integer
       (lambda (x) 
         (if (integer? x) (tag x) (error "Not an integer"))))
  (put 'eqzero '(integer) (lambda (x) (= 0 x)))
  (put 'exp '(integer integer)
       (lambda (x y) 
           (tag (expt x y))))
  (put 'sqrt '(integer) (lambda (x) (sqrt x)))
  (put 'atan '(integer integer) (lambda (x y) (atan x y)))
  (put 'sin '(integer) (lambda (x) (sin x)))
  (put 'cos '(integer) (lambda (x) (cos x)))
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'project '(integer) (lambda (x) x))
  'done)

(install-integer-package)
(define (make-integer n) ((get 'make 'integer) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (sqrt-rat x) (make-rat 
                         (numerator (inexact->exact (sqrt (/ (numer x) (denom x))))) 
                         (denominator (inexact->exact (sqrt (/ (numer x) (denom x)))))))
  (define (apply-helper-rat proc x y)
          (let ((newx (/ (numer x) (denom x))) (newy (/ (numer y) (denom y))))
            (let ((result (inexact->exact (proc newx newy))))
              (make-rat (numerator result) (denominator result)))))
  (define (apply-helper-rat-1 proc x)
          (let ((newx (/ (numer x) (denom x))))
            (let ((result (inexact->exact (proc newx))))
              (make-rat (numerator result) (denominator result)))))
  
  (define (atan-rat x y) 
          (apply-helper-rat atan x y))
  (define (equal-rat x y) (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  (define (=zero?-rat x) (= 0 (numer x)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'sqrt '(rational) (lambda (x) (tag (sqrt-rat x))))
  (put 'atan '(rational rational) (lambda (x y) (tag (atan-rat x y))))
  (put 'sin '(rational) (lambda (x) (tag (apply-helper-rat-1 sin x))))
  (put 'cos '(rational) (lambda (x) (tag (apply-helper-rat-1 cos x))))                          
  (put 'cos '(rational rational) (lambda (x y) (tag (apply-helper-rat cos x y))))
  (put 'equal '(rational rational)
       (lambda (x y) (equal-rat x y)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'eqzero '(rational) (lambda (x) (=zero?-rat x)))
  (put 'raise '(rational) (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'project '(rational) (lambda (x) (make-integer (round (/ (numer x) (denom x))))))
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (define (make-real r) (if (real? r) (tag r) (error "Not a real number")))
  (define (put-for-real op func) (put op '(real real) func))
  (put-for-real 'add (lambda (r1 r2) (tag (+ r1 r2))))
  (put-for-real 'sub (lambda (r1 r2) (tag (- r1 r2))))
  (put-for-real 'mul (lambda (r1 r2) (tag (* r1 r2))))
  (put-for-real 'div (lambda (r1 r2) (tag (/ r1 r2))))
  (put-for-real 'equal (lambda (r1 r2) (= r1 r2)))
  (put 'make 'real make-real)
  (put 'eqzero '(real) (lambda (r) (= 0 r)))
  (put 'raise '(real) (lambda (r) (make-complex-from-real-imag r 0)))
  (put 'project '(real) (lambda (r) (make-rational (inexact->exact (numerator r)) (inexact->exact (denominator r)))))
  'done)

(install-real-package)
(define (make-real r) ((get 'make 'real) r))

(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  (define (equal-complex z1 z2) (and (= (magnitude z1) (magnitude z2)) (= (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'equal '(complex complex) (lambda (z1 z2) (equal-complex z1 z2)))
  (put 'eqzero '(complex) (lambda (z) (equal-complex z (make-from-mag-ang 0 0))))
  (put 'project '(complex) (lambda (z) (make-real (real-part z))))
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (scheme-number->complex n)
  (make-complex-from-real-imag 
   (contents n) 0))

(define (drop num) 
  (let ((tt (type-tag num)))
    (if (eq? tt 'integer) 
      num 
      (let ((dnum (project num)))
        (if (equ? (raise dnum) num) (drop dnum) num)))))

(put-coercion 'scheme-number 'complex scheme-number->complex)