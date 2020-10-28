;; emg SICP chapter 3.4.2 stuff

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer 
         (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) 
             balance-serializer)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

; (define (make-semaphore-a n)
;     (let ((muxs make-list n (make-mutex)))
;         (define (get-one) 
;             ())
;         (define (the-semaphore m)
;             (cond ((eq? m 'acquire) (get-one))))))

(define (get-one l)
    (let get-one-helper ((listy l))
        (if (null? listy)
            (get-one-helper l) ;retry
            (let ((first (car listy)))
                (if (test-and-set! first)
                    (get-one-helper (cdr listy))
                    false)))))

(define (get-falses k)
    (if (= k 0) 
        '() 
        (let ((z (list false)))
            (cons z (get-falses (- k 1))))))

(define (cons-of-these k this)
    (if (= k 0) '() (cons this (cons-of-these (- k 1) this))))

(define (make-semaphore-b n)
    (let ((cells (get-falses n)))
        (define (the-semaphore m)
            (cond ((eq? m 'acquire) 
                    (get-one cells))
                  ((eq? m 'release) 
                    (set! cells (get-falses n)))
                  ((eq? m 'peep) cells)))
        the-semaphore))