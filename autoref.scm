(module autoref (make-table inc dec autoref-count)
(import chicken scheme)
(import pat)

(define (datomic? t)
  (or (string? t)
      (number? t)
      (char? t)))

(define (make-table symbols)
  (map (lambda (sym) (cons sym 0)) symbols))

(define (inc sym table)
  (cond ((assoc sym table) =>
         (lambda (entry)
           (set-cdr! entry (+ 1 (cdr entry)))
           table))
        (else table)))

(define (dec sym table)
  (cond ((assoc sym table) =>
         (lambda (entry)
           (set-cdr! entry (- (cdr entry) 1))
           table))
        (else table)))

(define (autoref-count t table)
  (cond ((symbol? t) (inc t table))
        ((datomic? t) table)
        ((and (pair? t) (symbol? (car t)))
         (case (car t)
           ((if) (if (= 4 (length t))
                     (autoref-count (cadr t)
                              (autoref-count (caddr t)
                                       (autoref-count (cadddr t) table)))
                     (error "bad if")))
           ((invoke-builtin invoke-toplevel invoke-closure)
            (foldr autoref-count table (cdr t)))
           (else (foldr autoref-count table t))))
        (else (error "I don't know what" t "is"))))

;; #;7> (autoref-count '(invoke-closure (invoke-closure x z) (invoke-closure y z)) (make-table '(x y z)))
;; ((x . 1) (y . 1) (z . 2))

)
