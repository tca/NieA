(module cc (perform-cc)
(import chicken scheme)
(import pat sets)

(define (list-index elt list)
  (define (list-index* elt list i)
    (if (null? list)
        #f
        (if (eq? elt (car list))
            i
            (list-index* elt (cdr list) (+ i 1)))))
  (list-index* elt list 0))

(define (concatenate lists)
  (apply append lists))

(define env-variable-name (gensym 'env)) ;; TODO gensym this AFTER reading file in (very important to avoid clashes)

(define (annotate-free-program t)
  (map annotate-free-definition t))
(define (annotate-free-definition d)
  (match d
    ((define formals body) =>
     (call-with-values (lambda () (annotate-free-term (car formals) (cdr formals) body))
       (lambda (free term)
         (unless (null? (set-remove free (cdr formals)))
           (error "toplevel definition" (car formals) "has free variables!"))
         `(define ,formals ,term))))
    (else (error (list "[impossible annotate] Not a valid definition at toplevel:" d)))))
(define (annotate-free-term def scope t)
  (cond ((symbol? t) (values (list t) t))
        ((number? t) (values '() t))
        ((string? t) (values '() t))
        ((and (list? t) (not (null? t)))
         (case (car t)
           ((lambda) (let ((bindings (cadr t))
                           (body (caddr t)))
                       (call-with-values (lambda () (annotate-free-term def (append bindings scope) body))
                         (lambda (free body)
                           (let ((frees (set-remove free bindings)))
                             (values frees `(lambda ,frees ,bindings ,body)))))))
           ((if) (let ((pred (cadr t))
                       (then (caddr t))
                       (else (cadddr t)))
                   (call-with-values (lambda () (annotate-free-term def scope pred))
                     (lambda (pred-free pred-term)
                       (call-with-values (lambda () (annotate-free-term def scope then))
                         (lambda (then-free then-term)
                           (call-with-values (lambda () (annotate-free-term def scope else))
                             (lambda (else-free else-term)
                               (values (set-union* (list pred-free then-free else-free))
                                       `(if pred-term then-term else-term))))))))))
           (else (let loop ((frees '())
                            (terms '())
                            (in-terms t))
                   (if (null? in-terms)
                       (values frees (reverse terms))
                       (call-with-values (lambda () (annotate-free-term def scope (car in-terms)))
                         (lambda (free term)
                           (loop (set-union free frees)
                                 (cons term terms)
                                 (cdr in-terms)))))))))
        (else (error "[impossible annotate] Not a valid term" t "inside" def))))

(define (cc-program t)
  (map cc-definition t))
(define (cc-definition d)
  (match d
    ((define formals body) => `(define ,(cons (car formals) (cons env-variable-name (cdr formals))) ,(cc-term (car formals) '() body)))
    (else (error (list "[impossible cc] Not a valid definition at toplevel:" d)))))
(define (cc-term def env t)
  (cond ((symbol? t)
         (cond ((list-index t env) => (lambda (i) `(vector-ref ,env-variable-name ,i)))
               (else t)))
        ((number? t) t)
        ((string? t) t)
        ((and (list? t) (not (null? t)))
         (case (car t)
           ((lambda) (let ((local-env (cadr t))
                           (bindings (caddr t))
                           (body (cadddr t)))
                       `(make-closure (lambda ,(cons env-variable-name bindings)
                                        ,(cc-term def local-env body))
                                      (vector . ,local-env))))
           ((if) (let ((pred (cadr t))
                       (then (caddr t))
                       (else (cadddr t)))
                   (let ((r (lambda (t) (cc-term def env t))))
                     `(if ,(r pred) ,(r then) ,(r else)))))
           (else (let ((r (lambda (t) (cc-term def env t))))
                   (cons 'invoke-closure (map r t))))))
        (else (error "[impossible] Not a valid term" t "inside" def))))


(define (perform-cc program)
  (set! env-variable-name (gensym 'env))
  (cc-program (annotate-free-program program)))


)
