(define (mk-g y) (lambda (x) (+ x y)))
(define (f g) (g 2))
(print (f (mk-g 3)))

