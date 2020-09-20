#lang scheme
(define (gcd a b)
    (if (= b 0) a (gcd b (remainder a b))))
(define (make-accumulator initial-amount)
    (lambda (n) 
        (begin (set! initial-amount (+ initial-amount n)) initial-amount)))

(define (make-monitored fn) 
    (let ((counter 0))
        (define (mf inp)
            (cond ((eq? inp 'how-many-calls?) counter)
                  ((eq? inp 'reset-count) (set! counter 0))
                  (else (begin (set! counter (+ counter 1)) (fn inp)))))
        mf))

(define (in-list? in list)
    (if (empty? list) '#f (if (eq? in (car list)) '#t (in-list? in (cdr list))))
)

(define (make-account balance pwd)
    (let ((access-counter 0) (other-passwords '()))
        (define (withdraw amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount)) balance)
                "Insufficient funds"))
        (define (deposit amount)
            (set! balance (+ balance amount))
            balance)
        (define (add-password new-password)
            (set! other-passwords (cons new-password other-passwords)))
        (define (dispatch p m)
            (if (or (eq? p pwd) (in-list? p other-passwords))
                (begin (set! access-counter 0)
                       (cond ((eq? m 'withdraw) withdraw)
                             ((eq? m 'deposit) deposit)
                             ((eq? m 'add-password) add-password)
                             (else (error "Unknown request: MAKE-ACCOUNT" m))))
                (if (> access-counter 2) 
                    (begin (call-the-cops) (error "Exceeded MAX-PASSWORD-INCORRECT!"))
                    ((set! access-counter (+ access-counter 1))
                     (error "Bad password: MAKE-ACCOUNT" m)))))
    dispatch))

(define (call-the-cops) (display "you did it now bud!"))

(define (make-joint-account first-account first-password second-password)
    (begin ((first-account first-password 'add-password) second-password) first-account)
)

(define random-max (expt 2 31))
(define random-init 13)
(define a 1664525)
(define b 1013904223)
(define m (expt 2 32))
(define rand-update (lambda (x) (remainder (+ (* a x) b) m)))

(define rand
  (let ((x random-init))
    (lambda (symbol) (cond ((eq? symbol 'generate) (begin (set! x (rand-update x)) x))
                           ((eq? symbol 'reset) (lambda (newvalue) (set! x newvalue)))
                           (else (error "Bad argument to rand (must be generate or reset [x]"))))))

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2)
    (define the-experiment 
        (lambda () (let ((ran-x (random-in-range x1 x2)) (ran-y (random-in-range y1 y2)))
                        (P ran-x ran-y))))
    (* (monte-carlo (expt 2 22) the-experiment) 4))

(define in-unit-100K-circle? (lambda (x y) (< (+ (expt x 2) (expt y 2)) (expt 100000 2))))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
   (= (gcd (random random-max) (random random-max)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

;; (+ (f 0) (f 1))
;; such that:
;; return 0 if args eval left to right but 1 if right to left
(define f 
    (let ((numcalls 0))
        (lambda (n) 
            (if (= n 0)
                (if (= numcalls 0)
                    (begin (set! numcalls (+ numcalls 1)) 0)
                    (begin (set! numcalls 0) 0))
                (if (= numcalls 0)
                    (begin (set! numcalls (+ numcalls 1)) 1)
                    (begin (set! numcalls 0) 0))))))