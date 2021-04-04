(define flatten
  (lambda (tree)
    (if (null? tree)
        '()
        (if (atom? tree)
            (list tree)
            (append (flatten (car tree))
                    (flatten (cdr tree)))))))

(trace flatten)

(flatten '((a b) c d (e f (g (h)))))
