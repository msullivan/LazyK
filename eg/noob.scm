(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")

(lazy-def 'two-plus-two
 '(+ 2 2))

(print-as-unlambda (laze 'two-plus-two))

