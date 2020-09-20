(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr  '()))
    (define (dispatch m) 
      (cond ((eq? m 'insert) 
              (lambda (z) 
                (let ((new-pair (cons z '())))
                  (if (null? front-ptr)
                    (begin
                      (set! front-ptr new-pair)
                      (set! rear-ptr new-pair)
                      (cons front-ptr rear-ptr)
                    )
                    (begin
                      (set-cdr! rear-ptr new-pair)
                      (set!     rear-ptr new-pair)
                      (cons front-ptr rear-ptr)
                    )
                  )
                )
              )
            )
            ((eq? m 'delete)
              (lambda ()
                (if (null? front-ptr) 
                  (error "delete on empty")
                  (begin (set! front-ptr (cdr front-ptr)) (cons front-ptr rear-ptr))
                )))
            (else (error "whatup"))
      )
    )
    dispatch))