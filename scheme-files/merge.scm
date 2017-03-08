(
    (define 
        (mergeSort l) 
        (if (null? l)
            null
            (if (null? (cdr l))
                (merge null null l)
                (let 
                    (
                        (middle (/ (- (length l 0) 1) 2)) 
                        (left (splitList l 0 middle 0))
                        (right (splitList l (+ middle 1) (- (length l 0) 1) 0))
                    )
                    (merge (mergeSort left) (mergeSort right) null)
                )
            )
        )
    )

    (define
        (length l acc)
        (if (null? l)
            acc
            (length (cdr l) (+ acc 1))
        )
    )

    (define 
        (splitList l low high acc)
        (if (> acc high)
            null 
            (if (< acc low)
                (splitList (cdr l) low high (+ acc 1))
                (cons (car l) (splitList (cdr l) low high (+ acc 1)) )

            ) 
        )
    )

    (define
        (merge l r s)
        (if (null? l)
            (if (null? r)
                s
                (merge null null (append s r))
            )
            (if (null? r)
                (merge null null (append s l))
                (if (< (car l) (car r))
                    (merge (cdr 1) r (append s (cons (car l) null)))
                    (merge l (cdr r) (append s (cons (car r) null)))
                )
            )
        )
    )

    (define
        (append l s)
        (if (null? l)
            s
            (cons (car l) (append (cdr l) s))
        ) 
    )

    (mergeSort (quote (2 8 4 1 10)))
)