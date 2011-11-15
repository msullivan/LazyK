;; Iota interpreter. Input to the Iota program begins
;; immediately after the last character of the Iota
;; program. Characters other than * and i are ignored
;; before that point.

(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")

(lazy-def '(main input) '(parse-iota i input))

(lazy-def 'parse-iota
 '((lambda (x) (x x))
   (lambda (self cont input)
     ((nth (car input) dispatch-list) (self self) cont (cdr input)) )))

(lazy-def 'dispatch-list
 '((42 (cons (lambda (parse cont input) (parse cont input))))
   (cons (lambda (parse cont) (parse (lambda (f1) (parse (lambda (f2) (cont (f1 f2)))))))
         ((62 (cons (lambda (parse cont input) (parse cont input))))
          (cons (lambda (parse cont) (cont iota))
                (list-of (lambda (parse cont input) (parse cont input))) )))))

(lazy-def '(iota x) '(x S K))

(print-as-iota (laze 'main))
