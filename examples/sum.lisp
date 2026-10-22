(define (sum n)
  (if (zero? n)
    0
    (+ n (sum (sub1 n)))))

(print (sum 1000000))