(define (cons car cdr)
 (lambda (selector)
  (selector car cdr)))
(define (car c)
 (c (lambda (x y) x)))
(define (cdr c)
 (c (lambda (x y) y)))

(define (begin x y) y)

(define (print-each list)
 (if list
     (begin (print (car list))
            (print-each (cdr list)))
     0))

(define (scm-main)
 (print-each (cons "Hello" (cons "World" (cons "!" 0)))))

