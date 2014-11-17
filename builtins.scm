(module builtins (builtins)
(import chicken scheme)

(define builtins
  '((print . scm-print)
    (= . scm-eq)
    (+ . scm-plus)))
)
