;; Eric Gullufsen
;; SICP Exercise 4.3 
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