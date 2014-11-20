(module builtins (builtins builtin-wrappers builtin-gensyms)
(import chicken scheme)

(define builtins1
  '((print (x) . scm-print)
    (= (x y) . scm-eq)
    (>= (x y) . scm-gteq)
    (<= (x y) . scm-lteq)
    (> (x y) . scm-gt)
    (< (x y) . scm-lt)
    (+ (x y) . scm-plus)
    (- (x y) . scm-minus)
    (* (x y) . scm-multiply)
    (/ (x y) . scm-divide)
    (vector-ref (vec idx) . scm-vector-ref)
    (vector-length (vec) . scm-vector-length)
    (make-vector (len gen) . scm-make-vector)
    ))

(define builtins (map (lambda (b) (list (car b) (map gensym (cadr b)) (gensym (cddr b)) (cddr b))) builtins1))

(define builtin-gensyms (map (lambda (b) (cons  (cadddr b) (caddr b))) builtins))

(define builtin-wrappers
  (map
   (lambda (b) 
     `(define (,(caddr b) . ,(cadr b)) (,(car b) . ,(cadr b))))
   builtins))

)
