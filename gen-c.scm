(module gen-c (gen-c)
(import chicken scheme)
(import pat)

(define (gen-c-expr e)
  (cond ((symbol? e) (list e))
        ((number? e) (list e))
        ((list? e)
         (match e
           ((vector-ref env i) => `((vector_ref ,env ,i)))
           ((closure fn env) => `((make_closure ,fn ,env)))
           ((invoke-closure fn (args args)) =>
            `((invoke_closure ,fn . ,(foldl append '() (map gen-c-expr args)))))
           (else (error (list "uknown exp: " e)))))
        (else (error (list "uknown exp: " e)))))


(define (compile-define formals body)
  (let ((ret-type '(* scm))
        (name (car formals))
        (args (map (lambda (a) `((* scm) ,a)) (cdr formals)))
        (refcounting '()))
    `(define (,ret-type ,name . ,args)
       . ,(append (gen-c-expr body)
                  refcounting))))

(define (gen-c-def d)
  (match d
    ((define formals body) => (compile-define formals body))))

(define (gen-c p)
  (map gen-c-def p))
)
