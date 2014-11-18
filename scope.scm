(module scope (scope)
(import chicken scheme)
(import pat)
(import builtins)

(define top-level '())

(define (rename! e renames)
  (let ((sym (gensym e)))
    (set-car! renames (cons (cons e sym) (car renames)))
    sym))

(define (foo bindings renames)
  (map (lambda (a)
         (cond ((assoc a renames) => cdr)
               (else a)))
       bindings))

(define (foo2 upper-bindings renames inner-renames)
  (foldr (lambda (c m) (cond ((member (car c) upper-bindings) (cons c m))
                             (else m)))
         renames inner-renames))

;; if a variable is in toplevel, rename it locally
(define (scope-form e env renames)
  (match e
    ((if pred then else) => `(if ,(scope-form pred env renames)
                                 ,(scope-form then env renames)
                                 ,(scope-form else env renames)))
    ((lambda params body) =>
       (let* ((inner-renames (list '()))
              (body (scope-form body (append params env) inner-renames))
              (params (foo params (car inner-renames)))
              (lifts (foo2 env (car renames) (car inner-renames))))
         (set-car! renames lifts)
       `(lambda ,params ,body)))
    (else (cond ((symbol? e) (if (member e env)
                                 (cond ((assoc e (car renames)) => cdr) ;; already renamed, use the new one
                                       ((member e top-level) (rename! e renames)) ;; var in top-level, rename!
                                       ((assoc e builtins) (rename! e renames)) ;; var is builtin, rename!
                                       (else e))
                                 e)) ;; dont need to do anything
                ((number? e) e)
                ((string? e) e)
                ((list? e) (map (lambda (e) (scope-form e env renames)) e))))))

(define (scope top-level1 p)
  (set! top-level top-level1)
  (map (lambda (d)
         (match d
           ((define formals body) => 
            (let* ((renames (list '()))
                   (body (scope-form body (cdr formals) renames))
                   (params (foo (cdr formals) (car renames))))
              
              `(define ,(cons (car formals) params) ,body)))))
       p))
)
