(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")


(lazy-def 'rot13
 '(mapcar (lambda (x)
            ((nth x translation-lookup-table) x) )))

(lazy-def '(mapcar func)
 '((lambda (x) (x x))
   (lambda (self lst)
     (cons (func (car lst))
           (self self (cdr lst)) ))))

(lazy-def 'translation-lookup-table
 '((lambda (rot-13)
     ((65 (cons i))
      (rot-13
       ((6 (cons i))
        (rot-13
         (list-of i) )))))
   (o (13 (cons (+ 13))) (13 (cons subtract-13))) ))

(lazy-def '(subtract-13 n)
 '(nth n ((13 (cons i)) (list-from 0))) )


(print-as-unlambda (laze 'rot13))
