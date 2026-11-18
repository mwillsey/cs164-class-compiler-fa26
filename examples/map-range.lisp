(define (range lo hi)
  (if (< lo hi)
   (pair lo (range (add1 lo) hi))
   false))
(define (map f l)
  (if (not l) l
  (pair (f (left l)) (map f (right l)))))
(define (g x) (+ x 10))
(print (map g (range 0 4)))