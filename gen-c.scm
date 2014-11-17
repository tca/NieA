(module gen-c (gen-c)
(import chicken scheme)
(import pat)

(define (push! box val)
  (set-car! box (cons val (car box))))

(define (struct-ref* val path)
  (foldl (lambda (m c) `(struct-ref ,m ,c)) val path))

(define (gen-c-expr e box)
  (cond ((symbol? e) e)
        ((number? e) (compile-int e))
        ((string? e) (compile-string e box))
        ((list? e)
         (match e
           ((if pred then else) => (compile-if pred then else box))
           ((vector-ref env i) => `(array-ref ,env ,i))
           ((make-closure fn env) => (compile-closure fn (cdr env) box))
           

           ;; ((invoke-closure cls args) =>
           ;;  `(((* (array-ref (struct-ref (* ,cls) elt) 0))
           ;;     (struct-ref (* ,cls) env)
           ;;     . ,(foldl append '() (map gen-c-expr args)))))
           
           (else (if (and (not (null? e))
                          (symbol? (car e)))
                     (let ((args (map (lambda (x) (gen-c-expr x box)) (cdr e))))
                       `(,(car e) . ,args))
                     (error (list "Not a proper functiona pplication" e))))))
        (else (error (list "uknown exp: " e)))))

;; set return variable
(define (compile-if pred then else box)
  (let ((retsym (gensym "if-result")))
    (push! box `(declare (struct ,retsym) ,retsym))
    (push! box
    `(if ,(gen-c-expr pred box)
         (begin . ,(compile-body then (lambda (x) `(set! ,retsym ,x))))
         (begin . ,(compile-body else (lambda (x) `(set! ,retsym ,x))))))
    retsym))

(define (compile-int i)
  `(make-struct (struct scm) (tag 0) (val.i ,i)))

(define (compile-build-array sym elts box)
  (define (loop vs i m)
    (if (null? vs)
        (append `((declare (struct scm) ,sym)
                  (set! ,sym (allocate-vector ,i)))
                (reverse m))
        (let ((expr `(set! (array-ref (struct->ref ,(struct-ref* sym '(val v)) elt) ,i)
                           ,(gen-c-expr (car vs) box))))
          (loop (cdr vs) (+ i 1) (cons expr m)))))
  (for-each (lambda (e) (push! box e)) (loop elts 0 '()))
  sym)

(define (compile-string str box)
  (compile-build-array (gensym "str") (map char->integer (string->list str)) box))

(define (compile-closure fn env box)
  (define sym (gensym "new-env"))
  (compile-build-array sym (cons `(& ,fn) env) box)
  `(make-closure ,sym))

(define (compile-body body k)
  (let ((box (list '())))
    (let ((c-body (gen-c-expr body box))
          (refcounting '()))
      (append (reverse (car box))
              refcounting
              (list (k c-body))))))

(define (compile-define formals body)
  (let ((ret-type '(struct scm))
        (name (car formals))
        (args (map (lambda (a) `((struct scm) ,a)) (cdr formals))))
    `(define (,ret-type ,name . ,args)
       . ,(compile-body body (lambda (x) `(return ,x))))))

(define (gen-c-def d)
  (match d
    ((define formals body) => (compile-define formals body))))

(define (gen-c p)
  (map gen-c-def p))
)



