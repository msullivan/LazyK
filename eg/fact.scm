(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")

(lazy-def '(fact n)
 '(cdr
   (n (lambda (p)
        (cons (1+ (car p)) ((* (cdr p)) (car p))))
      (cons 1 1))))

(lazy-def 'fact-5 '(fact 5))

(print-as-cc (laze 'fact-5))
      
