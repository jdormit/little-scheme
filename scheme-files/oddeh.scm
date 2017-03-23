(
 (define (even? n)
   (if (equal? n 0)
       #t
       (odd? (- n 1))))
 (define (odd? n)
   (if (equal? n 0)
       #f
       (even? (- n 1))))
 (even? 10)
 )
