(module niea (valid-program? validate-program)
(import chicken scheme extras (srfi 1))
(import pat)

(define (all p l)
  (if (null? l)
      #t
      (if (p (car l))
          (all p (cdr l))
          #f)))

;; Language definition

;; <program> ::= <definition>*
;; <definition> ::= (define (<symbol> <symbol>) <term>)
;; <term> ::= <symbol>
;;          | (<term> <term>)
;;          | (lambda (<symbol>) <term>)
;;          | <number>
;;          | (if <term> <term> <term>)

;; Validator with errors

(define (valid-program? t)
  (all valid-definition? t))
(define (valid-definition? d)
  (match d
    ((define formals body) => (and (list? formals)
                                   (= 2 (length formals))
                                   (symbol? (first formals))
                                   (symbol? (second formals))
                                   (valid-term? (first formals) body)))
    (else (error (list "Not a valid definition at toplevel:" d)))))
(define (valid-term? def t)
  (unless (or (symbol? t)
              (and (= 2 (length t))
                   (valid-term? def (first t))
                   (valid-term? def (second t)))
              (number? t)
              (match t
             ;;   ((begin a b) => (and (valid-term? def a)
             ;;                      (valid-term? def b)))
                ((lambda formals body) => (and (list? formals)
                                               (= 1 (length formals))
                                               (valid-term? def body)))
                ((if pred then else) => (and (valid-term? def pred)
                                             (valid-term? def then)
                                             (valid-term? def else)))
                (else #f)))
    (error (list "Not a valid term" t "inside" def))))

(define definitions '())
(define (collect-definition e)
  (match e
    ((define formals body) =>
     (set! definitions (cons (car formals) definitions)))))
(define (collect-definitions exprs)
  (set! definitions '())
  (for-each collect-definition exprs)
  definitions)
(define (check-scope e env)
  (match e
  ;;  ((begin a b) => (check-scope a env) (check-scope b env))
    ((if pred then else) => (for-each (lambda (e) (check-scope e env))
                                      (cdr e)))
    ((lambda params body) => (check-scope body (append params env)))
    (else (cond ((symbol? e)
                 (unless (member e env)
                    (error (list "Unbound variable: " e))))
                ((number? e) '())
                ((list? e) (for-each (lambda (e) (check-scope e env))
                                     e))
                (else #t)))))
(define (well-scoped? p)
  (let ((defs (collect-definitions p)))
    (for-each (lambda (d)
                (match d
                  ((define formals body) => 
                   (check-scope body (append (cdr formals) defs)))))
              p)))
                 

(define (validate-program filename)
  (let ((program (read-file filename)))
    (valid-program? program)
    (well-scoped? program)
    (print (list filename "is a valid program!"))))


)
