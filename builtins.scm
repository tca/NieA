(module builtins (builtins builtin-wrappers builtin-gensyms)
(import chicken scheme)

(define builtins1
  '((print (x) . scm-print)
    (= (x y) . scm-eq)
    (+ (x y) . scm-plus)))

(define builtins (map (lambda (b) (list (car b) (map gensym (cadr b)) (gensym (cddr b)) (cddr b))) builtins1))

(define builtin-gensyms (map (lambda (b) (cons  (cadddr b) (caddr b))) builtins))

(define builtin-wrappers
  (map
   (lambda (b) 
     `(define (,(caddr b) . ,(cadr b)) (,(car b) . ,(cadr b))))
   builtins))

)
