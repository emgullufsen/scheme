;; code from the book, with some additions 
(define apply-in-underlying-scheme apply)

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure
	  procedure
	  (list-of-arg-values arguments env)))  ; changed
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   (list-of-delayed-args arguments env) ; changed
	   (procedure-environment procedure))))
	(else
	 (error
	  "Unknown procedure type -- APPLY" procedure))))

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted? exp) 
         (text-of-quotation exp))
        ((assignment? exp) 
         (eval-assignment exp env))
        ((definition? exp) 
         (eval-definition exp env))
        ((if? exp) 
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((and? exp)
          (eval (and->if exp) env))
        ((or? exp)
          (eval (or->if exp) env))
        ((let? exp)
          (eval (let->combination exp) env))
        ((let*? exp)
	 (eval (let*->nested-lets exp) env))
	((application? exp)
	 (apply (actual-value (operator exp) env)
		(operands exp)
		env))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

;; thunk stuff

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
	    (list-of-arg-values (rest-operands exps)
				env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
	    (list-of-delayed-args (rest-operands exps)
				  env))))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
	 (let ((result (actual-value
			(thunk-exp obj)
			(thunk-env obj))))
	   (set-car! obj 'evaluated-thunk)
	   (set-car! (cdr obj) result)  ; replace `exp' with its value
	   (set-cdr! (cdr obj) '())     ; forget unneeded `env'
	   result))
	((evaluated-thunk? obj)
	 (thunk-value obj))
	(else obj)))


(define (actual-value exp env)
  (force-it (eval exp env)))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values 
             (rest-operands exps) 
             env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) 
                        env))))

(define (eval-assignment exp env)
  (set-variable-value! 
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable! 
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda 
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (begin? exp) 
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond-arrow-clause? c)
  (if (null? (cddr c))
    #f
    (if (eq? (cadr c) '=>)
      #t
      #f)))

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: COND->IF"
                       clauses))
            (if (cond-arrow-clause? first)
              (make-if (cond-predicate first)
                     (list (car (cddr first))             ;; applying the 'recipient' function
                       (cond-predicate first)) ;; with the result of predicate as arg
                     (expand-clauses 
                      rest))
              (make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest)))))))

(define (expand-clauses-old clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses-old 
                      rest))))))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? '*unassigned* (car vals))
              (error "var value is UNASSIGNED")
              (car vals)))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable SUP" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) 
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '* *)
        (list '+ +)
        (list '- -)
	(list '/ /)
        (list 'square (lambda (x) (* x x)))
        (list 'cadr cadr)
        (list 'assoc assoc)
        (list 'display display)
        (list '= =)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) 
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment 
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment 
  (setup-environment))

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
    (cons (make-lambda 
            (get-vars (car bindsNbod))
            (cdr bindsNbod))
          (get-exps (car bindsNbod))))

(define (expand-let-to-lambda-exp exp) (expand-let-to-lambda (cdr exp)))

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
            fbod
            (make-let (list (car binds)) (unroll-lets (cdr binds) fbod))))
    (unroll-lets (cadr exp) (car (cddr exp))))

; Let selectors
(define (let-initials exp) (map cadr (cadr exp)))
(define (let-parameters exp) (map car (cadr exp)))
(define named-let-identifier car)
(define let-body cddr)
 
; A named let is equivalent to a procedure definition 
; followed by a single application of that procedure with the 
; initial values given by the let expression.
;
; exp should be the initial let expression stripped of the 'let symbol
;   this allows the same selection procedures to be used without altering them.
(define (named-let->combination exp)
  (let ((procedure-name (named-let-identifier exp)))
    ; 2 expressions are needed so wrap them in a begin form
    (make-begin 
     (list
      ; define the procedure with the name given in the let expression
      (list 'define procedure-name 
            (make-lambda 
             (let-parameters exp) 
             (let-body exp)))
      ; apply the procedure with the initial values given by the let expression
      (cons procedure-name (let-initials exp))))))

(define (let->combination exp)
    (if (named-let? exp)
        (named-let->combination (cdr exp))
        (expand-let-to-lambda-exp exp)))

(define (named-let? exp)
    (if (tagged-list? exp 'let)
        (if (null? (cdddr exp))
            #f
            (if (symbol? (cadr exp)) #t #f))
        #f))

;;     '(let fib-iter ((a 0) (b 1)) (display "yay"))
(define (named-let->let nm)
    (make-let
        (cons (list (cadr nm) (make-lambda (get-vars (caddr nm)) (cdddr nm)))
              (caddr nm))
        (cadddr nm)))

(define (get-defines pb d)
    (if (null? pb)
        d
        (if (definition? (car pb))
            (get-defines (cdr pb) (cons (car pb) d))
            (get-defines (cdr pb) d))))

(define (get-non-defines pb d)
    (if (null? pb)
        d
        (if (definition? (car pb))
            (get-non-defines (cdr pb) d)
            (get-non-defines (cdr pb) (cons (car pb) d)))))

(define (defs->binds defs)
    (map (lambda (d) (list (definition-variable d) '(quote *unassigned*))) defs))

(define (make-assignment var val) (list 'set! var val))

(define (add-sets defs procbod) (append (map (lambda (d) (make-assignment (definition-variable d) (definition-value d))) defs) (get-non-defines procbod '())))

(define (scan-out-defines procbod)
    (let ((defs (get-defines procbod '())))
        (if (null? defs)
            procbod
            (list (append (list 'let
                    (defs->binds defs))
                    (add-sets defs procbod))))))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))


(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
	   (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
