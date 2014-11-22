(module gen-c (gen-c)
(import chicken scheme)
(import pat)
(import builtins autoref)

(define top-level '())

(define (push! box val)
  (set-car! box (cons val (car box))))

(define (struct-ref* val path)
  (foldl (lambda (m c) `(struct-ref ,m ,c)) val path))

(define (anfize e box)
  (let ((rsym (gensym "cls")))
    (push! box `(declare (struct scm) ,rsym))
    (push! box `(set! ,rsym ,e))
    rsym))

(define (gen-c-expr e box table)
  (cond ((symbol? e)
         (cond ((assoc e builtin-gensyms) =>
                (lambda (e) (compile-passed-builtin (cdr e) box table)))
               ((member e top-level) (compile-passed-builtin e box table))
               (else e)))
        ((number? e) (compile-int e))
        ((string? e) (compile-string e box table))
        ((char? e) (compile-int (char->integer e)))
        ((list? e)
         (match e
           ((vector-ref vec i) => (compile-vector-ref vec i box table))
           ((make-closure fn env) => (compile-closure fn (cdr env) box table))
           (else (if (or (null? e) (not (symbol? (car e))))
                     (error (list "Not a proper functiona pplication" e))
                     (case (car e)
                       ((invoke-builtin)
                        (anfize (compile-invoke-builtin  (cdr e) box table) box))
                       ((invoke-toplevel)
                        (anfize (compile-invoke-toplevel  (cdr e) box table) box))
                       ((invoke-closure)
                        (anfize (compile-invoke-closure  (cdr e) box table) box))
                       (else  (anfize (compile-application e box table) box)))))))
         (else (error (list "uknown exp: " e)))))

(define (compile-passed-builtin e box table)
  (compile-closure e '() box table))
           
(define (compile-vector-ref v i box table)
  `(array-ref (struct->ref ,(struct-ref* (gen-c-expr v box table) '(val v)) elt) ,i))

(define (compile-application e box table)
  (let ((args (map (lambda (x) (gen-c-expr x box table)) (cdr e))))
    `(,(car e) . ,args)))

;; compile without symbol conversions?
;; inline?
(define (compile-invoke-builtin args box table)
  `(,(car args)  . ,(map (lambda (x) (gen-c-expr x box table)) (cdr args))))

(define (compile-invoke-toplevel args box table)
  (define sym (gensym "empty_env"))
  (compile-build-array sym '() box table)
  `(,(car args) (make-closure ,sym) . ,(map (lambda (x) (gen-c-expr x box table)) (cdr args))))

(define (compile-invoke-closure args box table)
  (let ((sym (gensym "fn")))
    (push! box `(declare (type scm-fptr) ,sym))
    (push! box `(set! ,sym
                      (struct-ref (struct-ref (array-ref (struct->ref ,(struct-ref* (gen-c-expr (car args) box table) '(val v)) elt) 0) val) f)))
    `(,sym . ,(map (lambda (x) (gen-c-expr x box table)) args))))


(define (compile-int i)
  `(make-struct (struct scm) (tag 0) (val.i ,i)))

(define (compile-build-array sym elts box table)
  (define (loop vs i m)
    (if (null? vs)
        (append `((declare (struct scm) ,sym)
                  (set! ,sym (allocate-vector ,i)))
                (reverse m))
        (let ((expr `(set! (array-ref (struct->ref ,(struct-ref* sym '(val v)) elt) ,i)
                           ,(car vs)))
              (rc-inc `(refcount-inc (array-ref (struct->ref ,(struct-ref* sym '(val v)) elt) ,i))))
          (loop (cdr vs) (+ i 1) (cons rc-inc (cons expr m))))))
  (for-each (lambda (e) (push! box e)) (loop elts 0 '()))
  sym)

(define (compile-string str box table)
  (compile-build-array
   (gensym "str")
   (map (lambda (e) (gen-c-expr e box table)) (map char->integer (string->list str)))
   box table))

(define (compile-closure fn env box table)
  (define sym (gensym "new-env"))
  (define rsym (gensym "cls"))
  (compile-build-array
   sym
   (cons `(make-struct (struct scm) (tag 1) (val.f ,fn)) (map (lambda (x) (gen-c-expr x box table)) env))
   box table)
  (push! box `(declare (struct scm) ,rsym))
  (push! box `(set! ,rsym (make-closure ,sym)))
  rsym)

(define (compile-body params body k)
  (let ((box (list '()))
        (allocs (list '()))
        (table (autoref-count body (make-table params)))
        (retsym (gensym "return")))
    (let* ((incs (foldr (lambda (p m)
                          (if (> (cdr p) 1)
                              (cons `(refcount-inc* ,(car p) ,(cdr p)) m)
                              m))
                        '() table))
           (c-body (gen-c-expr body box table)))
      (append
       incs
       (reverse (car box))
       `((declare (struct scm) ,retsym)
         (set! ,retsym ,c-body))
       (foldr (lambda (a m)
                (cond ((> (cdr a) 0) m)
                      ((eq? (car a) c-body) m)
                      (else (cons `(refcount-dec ,(car a)) m))))
              '() table)
       (list (k retsym))))))

(define (compile-define formals body)
  (let ((ret-type '(struct scm))
        (name (car formals))
        (args (map (lambda (a) `((struct scm) ,a)) (cdr formals))))
    `(define (,ret-type ,name . ,args)
       . ,(compile-body (cdr formals) body (lambda (x) `(return ,x))))))

(define (gen-c-def d)
  (match d
    ((define formals body) => (compile-define formals body))))

(define (gen-c top-level1 p)
  (set! top-level top-level1)
  (map gen-c-def p))
)
