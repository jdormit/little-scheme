(
 (define
   (length l acc)
   (if (null? l)
       acc
       (length (cdr l) (+ acc 1))
       )
   )
 (print (length (quote (1 2 3 4)) 0))
 )
