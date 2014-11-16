(module gen-c (gen-c)
(import chicken scheme)
(import pat)

(define (push! box val)
  (set-car! box (cons val (car box))))

(define (gen-c-expr e box)
  (cond ((symbol? e) (list e))
        ((number? e) (list e))
        ((list? e)
         (match e
           ((vector-ref env i) => `((array-ref ,env ,i)))
           ((make-closure fn env) => (compile-closure fn (cdr env) box))
           

           ;; ((invoke-closure cls args) =>
           ;;  `(((* (array-ref (struct-ref (* ,cls) elt) 0))
           ;;     (struct-ref (* ,cls) env)
           ;;     . ,(foldl append '() (map gen-c-expr args)))))
           
           (else (if (and (not (null? e))
                          (symbol? (car e)))
                     (let ((args (map (lambda (x) (gen-c-expr x box)) (cdr e))))
                       ;; ERROR: this doesn't handle when args are longer than one element
                       (list `(,(car e) . ,(map car args))))
                     (error (list "Not a proper functiona pplication" e))))))
        (else (error (list "uknown exp: " e)))))

(define (compile-closure fn env box)
  (define sym (gensym "new-env"))
  (define (loop vs i m)
    (if (null? vs)
        (append `((declare (* (struct scm)) ,sym)
                  (set! ,sym (allocate-vector ,i)))
                (reverse m))
        (loop (cdr vs) (+ i 1)
              (cons `(set! (array-ref (struct-ref (* ,sym) elt) ,i) ,(car vs)) m))))
  (for-each (lambda (e) (push! box e)) (loop env 0 '()))
  `((make-closure ,fn ,sym)))

(define (compile-define formals body)
  (let ((ret-type '(* (struct scm)))
        (name (car formals))
        (args (map (lambda (a) `((* (struct scm)) ,a)) (cdr formals)))
        (box (list '())))
    (let ((body  (gen-c-expr body box))
          (refcounting '()))
    
    `(define (,ret-type ,name . ,args)
       . ,(append (reverse (car box))
                  body
                  refcounting)))))

(define (gen-c-def d)
  (match d
    ((define formals body) => (compile-define formals body))))

(define (gen-c p)
  (map gen-c-def p))
)



