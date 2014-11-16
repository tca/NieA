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
                ((lambda formals body) => (and (list? formals)
                                               (= 1 (length formals))
                                               (valid-term? def body)))
                ((if pred then else) => (and (valid-term? def pred)
                                             (valid-term? def then)
                                             (valid-term? def else)))
                (else #f)))
    (error (list "Not a valid term" t "inside" def))))

(define (validate-program filename)
  (when (valid-program? (read-file filename))
    (print (list filename "is a valid program!"))))

)
