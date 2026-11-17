
(define (f x)
  (add1 (add1 5)))

(define (f x)
  (add1 (add1 x)))

(define (f)
  (let ((x (+ 5 5))) (add1 x)))

(add1 5)