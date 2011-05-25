(load "../lazier.scm")
(load "../prelude.scm")

(lazy-def 'rev
 '((lambda (f) (f end-of-output))
     ((lambda (x) (x x))
      (lambda (self dst src)
        (if>= (car src) 256
              dst
              (self self (cons (car src) dst) (cdr src)) )))))

(print-as-jot (laze 'rev))
