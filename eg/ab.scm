(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")

(lazy-def '(main input)
 '(Y (o (cons 65) (cons 66))) )

(print-as-cc (laze 'main))
