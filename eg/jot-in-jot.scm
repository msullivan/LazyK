;; Jot interpreter. Input to the Jot program begins with
;; the first non-Jot character (even whitespace!)

(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")

(lazy-def '(main input) '(parse-jot i input))

(lazy-def 'parse-jot
 '((lambda (x) (x x))
   (lambda (self f input)
     ((nth (car input) dispatch-list) (self self) f input) )))

(lazy-def 'dispatch-list
 '((48 (cons (lambda (parse f input) (f input))))
   (cons (lambda (parse f input) (parse ((f s) k) (cdr input)))
         (cons (lambda (parse f input) (parse (s (k f)) (cdr input)))
               (list-of (lambda (parse f input) (f input))) ))))

(print-as-jot (laze 'main))
