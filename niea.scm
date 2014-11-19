;; rlwrap csi pat.scm sets.scm builtins.scm c-expr.scm scope.scm cc.scm hoist.scm gen-c.scm niea.scm

(module niea (valid-program? validate-program compile-program runtime)
(import chicken scheme extras (srfi 1))
(import pat c-expr)
(import builtins scope cc hoist gen-c)

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
;;          | <number> | <string> | <char>
;;          | (if <term> <term> <term>)

;; Validator with errors

(define (valid-program? t)
  (all valid-definition? t))
(define (valid-definition? d)
  (match d
    ((define formals body) => (unless (and (list? formals)
                                           (>= (length formals) 1)
                                           (all symbol? formals)
                                           (valid-term? (car formals) body))
                                (error "Invalid definition at toplevel: " formals)))
    (else (error (list "Not a valid definition at toplevel:" d)))))
(define (valid-term? def t)
  (unless (or (symbol? t)
              (string? t)
              (number? t)
              (char? t)
              (and (list? t)
                   (>= (length t) 1)
                   (all (lambda (tm) (valid-term? def tm)) t))
              (match t
             ;;   ((begin a b) => (and (valid-term? def a)
             ;;                      (valid-term? def b)))
                ((lambda formals body) => (and (list? formals)
                                               (>= (length formals) 0)
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
(define (check-scope def e env)
  (match e
  ;;  ((begin a b) => (check-scope a env) (check-scope b env))
    ((if pred then else) => (for-each (lambda (e) (check-scope def e env))
                                      (cdr e)))
    ((lambda params body) => (check-scope def body (append params env)))
    (else (cond ((symbol? e)
                 (unless (member e env)
                    (error (list "Unbound variable:" e "in" def))))
                ((number? e) '())
                ((string? e) '())
                ((char? e) '())
                ((list? e) (for-each (lambda (e) (check-scope def e env))
                                     e))
                (else #t)))))
(define (well-scoped? p)
  (let ((defs (append (map car builtins) (collect-definitions p))))
    (for-each (lambda (d)
                (match d
                  ((define formals body) => 
                   (check-scope (car formals) body (append (cdr formals) defs)))))
              p)
    defs))

(define (validate-program filename)
  (let ((program (read-file filename)))
    (valid-program? program)
    (let ((top-level (well-scoped? program)))
      (print (list filename "is a valid program!"))
      (let* ((hoisted (hoist (perform-cc top-level (scope top-level program))))
             (c-code (gen-c top-level hoisted)))
        (display ";; CC and Hoist") (newline)
        (for-each pretty-print hoisted)
        (newline)
        (display ";; c-exprs") (newline)
        (for-each pretty-print c-code)
        (newline)
        (display ";; c code") (newline)
        (display-c-program #f c-code)))
    ))

(define (compile-program filename)
   (let ((program (read-file filename)))
    (valid-program? program)
    (let ((top-level (well-scoped? program)))
      (let* ((hoisted (hoist (perform-cc top-level (scope top-level program))))
             (c-code (gen-c top-level hoisted)))
        (display-c-program #f (list `(include "stdio.h")
                                    `(include "stdlib.h")
                                    `(include "assert.h")
                                    `(include "runtime.h")
                                    `(include "builtins.h")))
        (display-c-program #t (append c-code
                                      (list
                                       `(define (int main (int argc) ((* (* char)) argv))
                                          (scm-main (make-struct (struct scm) (tag 0)))
                                          (return 0)))))))
    ))

(define (runtime)
  (let ((runtime-code (read-file "runtime.cexpr")))
    (display-c-program #f runtime-code)))

)
