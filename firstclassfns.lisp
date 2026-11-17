(define (add_one x) (+ x 1))
(define (add_two x) (+ x 2))
(let ((p (pair add_one add_two)))
  (print ((left p) 2)))