(
 (define 
   (mergeSort l1) 
   (if (null? l1)
       null
       (if (null? (cdr l1))
           (merge null null l1)
           (let 
               (
                (middle (/ (- (length l1 0) 1) 2)) 
                (left (splitList l1 0 middle 0))
                (right (splitList l1 (+ middle 1) (- (length l1 0) 1) 0))
                )
             (merge (mergeSort left) (mergeSort right) null)
             )
           )
       )
   )

 (define
   (length l2 acc1)
   (if (null? l2)
       acc1
       (length (cdr l2) (+ acc1 1))
       )
   )

 (define 
   (splitList l3 low high acc2)
   (if (> acc2 high)
       null 
       (if (< acc2 low)
           (splitList (cdr l3) low high (+ acc2 1))
           (cons (car l3) (splitList (cdr l3) low high (+ acc2 1)) )

           ) 
       )
   )

 (define
   (merge left right result)
   (if (null? left)
       (if (null? right)
           result
           (let ((newRes (append result right))) (merge null null newRes))
           )
       (if (null? right)
           (let ((newRes1 (append result left))) (merge null null newRes1))
           (if (< (car left) (car right))
               (let ((newRes2 (append result (cons (car left) null))))
                 (merge (cdr left) right newRes2)
                 )
               (let ((newRes3 (append result (cons (car right) null))))
                 (merge left (cdr right) newRes3)
                 )
               )
           )
       )
   )

 (define
   (append l5 s)
   (if (null? l5)
       s
       (cons (car l5) (append (cdr l5) s))
       ) 
   )

 (mergeSort (quote (2 8 4 1 10)))
 )
