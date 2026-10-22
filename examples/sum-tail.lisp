(define (sum-tail n total)
  (if (zero? n)
    total
    (sum-tail (sub1 n) (+ total n))))

(print (sum-tail 1000000 0))