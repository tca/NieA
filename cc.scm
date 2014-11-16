(module cc (annotate-free-program)
(import chicken scheme)
(import pat sets)

(define (concatenate lists)
  (apply append lists))

(define (annotate-free-program t)
  (map annotate-free-definition t))
(define (annotate-free-definition d)
  (match d
    ((define formals body) =>
     (call-with-values (lambda () (annotate-free-term (car formals) (cdr formals) body))
       (lambda (free term)
         `(define ,free ,formals ,term))))
    (else (error (list "[impossible] Not a valid definition at toplevel:" d)))))
(define (annotate-free-term def scope t)
  (cond ((symbol? t) (values (list t) t))
        ((number? t) (values '() t))
        ((and (list? t) (not (null? t)))
         (case (car t)
           ((lambda) (let ((bindings (cadr t))
                           (body (caddr t)))
                       (call-with-values (lambda () (annotate-free-term def (append bindings scope) body))
                         (lambda (free body)
                           (values (set-remove free bindings) `(lambda ,bindings ,body))))))
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
        (else (error "[impossible] Not a valid term" t "inside" def))))

;; (define (cc-program t)
;;   (map cc-definition t))
;; (define (cc-definition d)
;;   (match d
;;     ((define formals body) => `(define ,formals (cc-term (car formals) (cdr formals) body))
;;      (error "Invalid definition at toplevel: " formals))
;;     (else (error (list "[impossible] Not a valid definition at toplevel:" d)))))
;; (define (cc-term def scope t)
;;   (cond ((symbol? t) t)
;;         ((number? t) t)
;;         ((and (list? t) (not (null? t)))
;;          (case (car t)
;;            ((lambda) (let ((bindings (cadr t))
;;                            (body (caddr t)))
;;                        `(lambda ,bindings ,(cc-term def (append bindings scope) body))))
;;            ((if) (let ((pred (cadr t))
;;                        (then (caddr t))
;;                        (else (cadddr t)))
;;                    (let ((r (lambda (t) (cc-term def scope t))))
;;                      `(if ,(r pred) ,(r then) ,(r else)))))
;;            (else (let ((r (lambda (t) (cc-term def scope t))))
;;                    (map r t)))))
;;         (else (error "[impossible] Not a valid term" t "inside" def))))



)