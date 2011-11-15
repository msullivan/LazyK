(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")

(lazy-def '(fib input)
 '((lambda (x) (x x i (cons 42)))
   (lambda (self a b)
     (a (cons 10 (self self b (o a b)))) )))

(print-as-iota (laze 'fib))
