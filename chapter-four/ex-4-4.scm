;; Eric Gullufsen
;; SICP EXERCISES - CHAPTER FOUR

;; EXERCISE 4.1
(define (list-of-values-left-to-right exps env)
    (if (no-operands? exps)
        '()
        (let* ((lov-firs (eval (first-operand exps env)))
               (lov-rest (list-of-values-left-to-right (rest-operands exps) env)))
            (cons lov-firs lov-rest))))

(define (list-of-values-right-to-left exps env)
    (if (no-operands? exps)
        '()
        (let* ((lov-rest (list-of-values-right-to-left (rest-operands exps) env))
               (lov-firs (eval (first-operand exps env))))
            (cons lov-firs lov-rest))))

;; EXERCISE 4.2
;; a) Lois' plan is wrong in that eval with dispatch on type thinking (define x 3)
;;    is a procedure application (application? will return true for that expression)
;; b) If we swap in the below syntax-selector (prefixed with 'l-') procedure for application? 
;;    (and other selectors for parts of exp, also w/ 'l-' prefix) inside of eval, we will have helped 
;;    Lois out and procedure syntax in evaluated lang will be
;;    (call + 2 3)
(define (l-application? exp) (tagged-list? exp 'call))
(define (l-operator exp) (cadr exp))
(define (l-operands exp) (cddr exp))

;; EXERCISE 4.3 
;; data-directed eval

;; need tables
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) 
         (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) 
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;; since we are using data-directed style all 
;; eval-ops take expression and environment now,
;; so need to 'generalize' some eval-ops to accept these args
;; need to just throw away env...
(define (text-of-quotation-new exp env)
  (text-of-quotation exp))

(define (make-procedure-new exp env)
    (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))

(define (eval-sequence-new exp env)
    (eval-sequence (begin-actions exp) env))

(define (eval-cond-new exp env)
    (eval (cond->if exp) env))

(define eval-op-table (make-table))
(define (put-eval-op key val)
    (insert! key val eval-op-table))

(define (get-eval-op exp)
    (if (pair? exp)
        (let ((funkio (lookup (car exp) eval-op-table)))           ;; data-directed dispatch on type
            (if funkio
                funkio
                (lambda (exp env)                                  ;; application -> use apply
                    (apply 
                        (eval (operator exp) env) 
                        (list-of-values (operands exp) env)))))
        (if (or (number? exp) (string? exp))                       ;; self-evaluating
            identity
            (if (symbol? exp)                                      ;; variable
                lookup-variable-value
                (error "No Available Operation")))))

(put-eval-op 'quote text-of-quotation-new)
(put-eval-op 'set!  eval-assignment)
(put-eval-op 'define eval-definition)
(put-eval-op 'if eval-if)
(put-eval-op 'lambda make-procedure-new)
(put-eval-op 'begin eval-sequence-new)
(put-eval-op 'cond eval-cond-new)

(define (eval-new exp env) ((get-eval-op exp) exp env))

;; EXERCISE 4.4

(define (and? exp) (tagged-list? exp 'and))
(define (and->if exp) (expand-and-exp (cdr exp)))
(define (expand-and-exp clauses)
    (if (null? clauses)
        'true
        (let ((f (car clauses)) (r (cdr clauses)))
            (if (null? r)
                (make-if f f 'false)
                (make-if f (expand-and-exp r) 'false)))))

(define (or? exp) (tagged-list? exp 'or))
(define (or->if exp) (expand-or-exp (cdr exp)))
(define (expand-or-exp clauses)
    (if (null? clauses)
        'false
        (make-if (car clauses) (car clauses) (expand-or-exp (cdr clauses)))))

;; EXERCISE 4.6

(define (get-vars bs)
    (if (null? bs)
        '()
        (cons (caar bs) (get-vars (cdr bs)))))

(define (get-exps bs)
    (if (null? bs)
        '()
        (cons (car (cdr (car bs))) (get-exps (cdr bs)))))

(define (let? exp) (tagged-list? exp 'let))

(define (expand-let-to-lambda bindsNbod)
    (list (make-lambda 
            (get-vars (car bindsNbod))
            (cadr bindsNbod))
          (get-exps (car bindsNbod))))

(define (expand-let-to-lambda-exp exp) (expand-let-to-lambda (cdr exp)))

(define letexp '(let ((a 1) (b 2)) (+ a b)))
(define condexp '(cond ((assoc b ((a 1) (b 2))) => cadr) (else false)))
(define namedlet '(let fib-iter ((a 0) (b 1)) (display "yay")))
(define namedlet2 '(let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
; (let* ((x 3)
;        (y (+ x 2))
;        (z (+ x y 5)))
;   (* x z))

; (let ((x 3))
;     (let ((y (+ x 2)))
;         (let ((z (+ x y 5)))
;             (* x z))))

(define (make-let binds bod) (list 'let binds bod))
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
    (define (unroll-lets binds fbod)
        (if (null? binds)
            (sequence->exp fbod)
            (make-let (list (car binds)) (unroll-lets (cdr binds) fbod))))
    (unroll-lets (cadr exp) (car (cddr exp))))

(define (let->combination exp)
    (if (named-let? exp)
        (let->combination (named-let->let exp))
        (expand-let-to-lambda-exp exp)))

(define (named-let? exp)
    (if (tagged-list? exp 'let)
        (if (null? (cdddr exp))
            #f
            #t)
        #f))

;;     '(let fib-iter ((a 0) (b 1)) (display "yay"))
(define (named-let->let nm)
    (make-let
        (cons (list (cadr nm) (make-lambda (get-vars (caddr nm)) (cadddr nm)))
              (caddr nm))
        (cadddr nm)))