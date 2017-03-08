(
 (define 
   (append l s)
   (if (null? l)
       s
       (cons (car l) (append (cdr l) s))
       )
   )
 (append (quote (6)) null)
 )
