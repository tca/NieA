(module cc (perform-cc)
(import chicken scheme)
(import pat sets)
(import builtins)

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

;; Annotate also renames builtins now
(define (annotate-free-program top-level t)
  (map (lambda (d) (annotate-free-definition top-level d)) t))
(define (annotate-free-definition top-level d)
  (match d
    ((define formals body) =>
     (call-with-values (lambda () (annotate-free-term top-level (car formals) (cdr formals) body))
       (lambda (free term)
         (unless (null? (set-remove free (cdr formals)))
           (error "toplevel definition" (car formals) "has free variables!" (set-remove free (cdr formals))))
         `(define ,formals ,term))))
    (else (error (list "[impossible annotate] Not a valid definition at toplevel:" d)))))
(define (annotate-free-term top-level def scope t)
  (cond ((symbol? t) (cond ((assoc t builtins) =>
                            (lambda (found)
                              (values '() `(scm-wrap-fptr ,(cadddr found)))))
                           ((member t top-level)
                            (values '() `(scm-wrap-fptr ,t)))
                           (else (values (list t) t))))
        ((number? t) (values '() t))
        ((string? t) (values '() t))
        ((char? t) (values '() t))
        ((and (list? t) (not (null? t)))
         (case (car t)
           ((lambda) (let ((bindings (cadr t))
                           (body (caddr t)))
                       (call-with-values (lambda () (annotate-free-term top-level def (append bindings scope) body))
                         (lambda (free body)
                           (let ((frees (set-remove free bindings)))
                             (values frees `(lambda ,frees ,bindings ,body)))))))

           ;; ((if)
           ;;  (let ((pred (cadr t))
           ;;        (then (caddr t))
           ;;        (else (cadddr t)))
           ;;    (let ((g (gensym "ignored")))
           ;;      (annotate-free-term top-level def scope
           ;;                          `(boole ,pred
           ;;                                  (lambda (,g) ,then)
           ;;                                  (lambda (,g) ,else))))))

           ((if) (let ((pred (cadr t))
                       (then (caddr t))
                       (else (cadddr t)))
                   (call-with-values (lambda () (annotate-free-term top-level def scope pred))
                     (lambda (pred-free pred-term)
                       (call-with-values (lambda () (annotate-free-term top-level def scope then))
                         (lambda (then-free then-term)
                           (call-with-values (lambda () (annotate-free-term top-level def scope else))
                             (lambda (else-free else-term)
                               (values (set-union* (list pred-free then-free else-free))
                                       `(if ,pred-term ,then-term ,else-term))))))))))
           
           (else (let loop ((frees '())
                            (terms '())
                            (in-terms t))
                   (if (null? in-terms)
                       (values frees (reverse terms))
                       (call-with-values (lambda () (annotate-free-term top-level def scope (car in-terms)))
                         (lambda (free term)
                           (loop (set-union free frees)
                                 (cons term terms)
                                 (cdr in-terms)))))))))
        (else (error "[impossible annotate] Not a valid term" t "inside" def))))

(define (cc-program top-level t)
  (map (lambda (d) (cc-definition top-level d)) t))
(define (cc-definition top-level d)
  (match d
    ((define formals body) => `(define ,(cons (car formals) (cons env-variable-name (cdr formals))) ,(cc-term top-level (car formals) '() body)))
    (else (error (list "[impossible cc] Not a valid definition at toplevel:" d)))))
(define (cc-term top-level def env t)
  (cond ((symbol? t)
         (cond ((list-index t env) => (lambda (i) `(scm-vector-ref 0 ,env-variable-name ,i))) ;; 0 is a dummy env
               (else t)))
        ((number? t) t)
        ((string? t) t)
        ((char? t) t)
        ((and (list? t) (not (null? t)))
         (case (car t)
           ((lambda) (let ((local-env (cadr t))
                           (bindings (caddr t))
                           (body (cadddr t)))
                       `(make-closure (lambda ,(cons env-variable-name bindings)
                                        ,(cc-term top-level def local-env body))
                                      (vector . ,(map (lambda (t) (cc-term top-level def env t)) local-env)))))

           ((scm-wrap-fptr) t)
           ((if) `(if ,(cc-term top-level def env (cadr t))
                      ,(cc-term top-level def env (caddr t))
                      ,(cc-term top-level def env (cadddr t))))


           
           (else (let ((r (lambda (t) (cc-term top-level def env t))))
                   (if (symbol? (car t))
                       (cond ((member (car t) top-level)
                              (error "never happens")
                              (cons (car t)
                                    (cons 0 ;; dummy env
                                          (map r (cdr t))))
                              ;;(map r t)
                              )
                             ((member (car t) (map cadddr builtins))
                              (error "never")
                              (cons (car t)
                                    (cons 0 ;; dummy env
                                          (map r (cdr t)))))
                             (else (cons 'invoke-closure (map r t))))
                             
                       (cons 'invoke-closure (map r t)))))))
        (else (error "[impossible] Not a valid term" t "inside" def))))


(define (perform-cc top-level program)
  (let ((program program))
    (set! env-variable-name (gensym 'env))
    (cc-program top-level (annotate-free-program top-level program))))


)
