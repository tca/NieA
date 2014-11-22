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

(define (gen-c-expr e box allocs table)
  (cond ((symbol? e)
         (cond ((assoc e builtin-gensyms) =>
                (lambda (e) (compile-passed-builtin (cdr e) box allocs table)))
               ((member e top-level) (compile-passed-builtin e box allocs table))
               (else e)))
        ((number? e) (compile-int e))
        ((string? e) (compile-string e box allocs table))
        ((char? e) (compile-int (char->integer e)))
        ((list? e)
         (match e
           ;;((if pred then else) => (compile-if pred then else box allocs table))
           ((vector-ref vec i) => (compile-vector-ref vec i box allocs table))
           ((make-closure fn env) => (compile-closure fn (cdr env) box allocs table))
           (else (cond
                  ((or (null? e) (not (symbol? (car e))))
                   (error (list "Not a proper functiona pplication" e)))
                  ((equal? 'invoke-builtin (car e))
                   (anfize (compile-invoke-builtin  (cdr e) box allocs table) box))
                  ((equal? 'invoke-toplevel (car e))
                   (anfize (compile-invoke-toplevel  (cdr e) box allocs table) box))
                  ((equal? 'invoke-closure (car e))
                    (anfize (compile-invoke-closure  (cdr e) box allocs table) box))
                  (else  (anfize (compile-application e box allocs table) box))))))
         (else (error (list "uknown exp: " e)))))

(define (compile-passed-builtin e box allocs table)
  (compile-closure e '() box allocs table))
           
(define (compile-vector-ref v i box allocs table)
  `(array-ref (struct->ref ,(struct-ref* (gen-c-expr v box allocs table) '(val v)) elt) ,i))

(define (compile-application e box allocs table)
  (let ((args (map (lambda (x) (gen-c-expr x box allocs table)) (cdr e))))
    `(,(car e) . ,args)))

;; compile without symbol conversions?
;; inline?
(define (compile-invoke-builtin args box allocs table)
  `(,(car args)  . ,(map (lambda (x) (gen-c-expr x box allocs table)) (cdr args))))

(define (compile-invoke-toplevel args box allocs table)
  (define sym (gensym "empty_env"))
  (compile-build-array sym '() box allocs table)
  `(,(car args) (make-closure ,sym) . ,(map (lambda (x) (gen-c-expr x box allocs table)) (cdr args))))

(define (compile-invoke-closure args box allocs table)
  (let ((sym (gensym "fn")))
    (push! box `(declare (type scm-fptr) ,sym))
    (push! box `(set! ,sym
                      (struct-ref (struct-ref (array-ref (struct->ref ,(struct-ref* (gen-c-expr (car args) box allocs table) '(val v)) elt) 0) val) f)))
    `(,sym . ,(map (lambda (x) (gen-c-expr x box allocs table)) args))))


(define (compile-int i)
  `(make-struct (struct scm) (tag 0) (val.i ,i)))

(define (compile-build-array sym elts box allocs table)
  (define (loop vs i m)
    (if (null? vs)
        (append `((declare (struct scm) ,sym)
                  (set! ,sym (allocate-vector ,i)))
                (reverse m))
        (let ((expr `(set! (array-ref (struct->ref ,(struct-ref* sym '(val v)) elt) ,i)
                           ,(car vs)))
              (rc-inc `(refcount-inc (array-ref (struct->ref ,(struct-ref* sym '(val v)) elt) ,i))))
          (loop (cdr vs) (+ i 1) (cons expr m) #;(cons rc-inc (cons expr m))))))
  (for-each (lambda (e) (push! box e)) (loop elts 0 '()))
  (push! allocs sym)
  sym)

(define (compile-string str box allocs table)
  (compile-build-array
   (gensym "str")
   (map (lambda (e) (gen-c-expr e box allocs table)) (map char->integer (string->list str)))
   box allocs table))

(define (compile-closure fn env box allocs table)
  (define sym (gensym "new-env"))
  (define rsym (gensym "cls"))
  (compile-build-array
   sym
   (cons `(make-struct (struct scm) (tag 1) (val.f ,fn)) (map (lambda (x) (gen-c-expr x box allocs table)) env))
   box allocs table)
  (push! box `(declare (struct scm) ,rsym))
  (push! box `(set! ,rsym (make-closure ,sym)))
  rsym)

;; ;; set return variable
;; (define (compile-if pred then else box allocs table)
;;   (let ((retsym (gensym "if-result")))
;;     (push! box `(declare (struct scm) ,retsym))
;;     (push! box
;;            `(if (scm-extract-truth ,(gen-c-expr pred box allocs table))
;;                 (begin . ,(compile-body then (lambda (x) `(set! ,retsym ,x))))
;;                 (begin . ,(compile-body else (lambda (x) `(set! ,retsym ,x))))))
;;     retsym))

(define (compile-body params body k)
  (let ((box (list '()))
        (allocs (list '()))
        (table (autoref-count body (make-table params)))
        (retsym (gensym "return")))
    (let ((c-body (gen-c-expr body box allocs table)))
      (append
       (map (lambda (p) `(refcount-inc* ,(car p) ,(cdr p))) table)
       (reverse (car box))
       `((declare (struct scm) ,retsym)
         (set! ,retsym ,c-body)
         #;(refcount-inc ,retsym))
       (foldr (lambda (a m)
                (if (eq? (car a) retsym)
                    (cons `(refcount-dec ,a) m)
                    m))
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
