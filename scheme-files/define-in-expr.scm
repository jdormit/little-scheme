(
 (define (concatExp exp1 exp2 acc)
   (if (null? exp1)
       (if (null? exp2)
           acc
           concatExp(null (cdr exp2) (cons (car exp2) acc))
           )
       (concatExp (cdr exp1) exp2 (cons (car exp1) acc))
       )
   )
 (concatExp (quote (
                    (define (not v) (if v #f #t))
                    )
                   ) (quote (
                             (not #f)
                             )
                            ) null)
 )
