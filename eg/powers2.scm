(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")

(lazy-def '(powers2 input)
 '((lambda (x) (x x (cons 42)))
   (lambda (self n)
     (n (cons 10 (self self (o n n)))) )))

(print-as-unlambda (laze 'powers2))
