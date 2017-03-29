((define (foldLeft f l acc)
      (if (null? l)
             acc
                  (foldLeft f (cdr l) (f (car l) acc))))
  (foldLeft + (quote (1 2 3)) 0))
