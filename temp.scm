; Higher-order functions and lambdas
;; define map function without using recursion or top-level helper function
(define map (f xs)
    (foldr (lambda (x acc) (cons (f x) acc)) '() xs)
)