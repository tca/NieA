(module hoist (hoist)
(import chicken scheme)
(import pat)

(define hoisted '())
(define (hoist-lambda! params body)
  (let* ((sym (gensym))
         (formals (append (list sym) params))
         (def `(define ,formals ,body)))
    (set! hoisted (cons def hoisted))
    sym))

(define (hoist-form e)
  (match e
    ((if pred then else) => `(if ,(hoist-form pred)
                                 ,(hoist-form then)
                                 ,(hoist-form else)))
    ((lambda params body) =>
     (hoist-lambda! params (hoist-form body)))
    (else (cond ((symbol? e) e)
                ((number? e) e)
                ((list? e) (map hoist-form e))))))

(define (hoist p)
  (set! hoisted '())
  (append (map (lambda (d)
                 (match d
                   ((define formals body) =>
                    `(define ,formals ,(hoist-form body)))))
               p)
          hoisted))
)
