(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")

(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")


(lazy-def '(print-fibs input)
 '(digitize-list fibs))


(lazy-def '(map2 op)
 '((lambda (x) (x x))
   (lambda (self)
     (lambda (a b)
       (cons (op (car a) (car b))
             (self self (cdr a) (cdr b)) )))))


(lazy-def 'digitize-list
 '((lambda (x) (x x))
   (lambda (self)
     (lambda (lst)
       (digitize-n (car lst)
                   (cons 10 (self self (cdr lst))) )))))

(lazy-def 'digitize-n
 '((lambda (x) (x x))
   (lambda (self)
     (lambda (n tail)
       (ifnonzero n
                  ((lambda (divrem)
                     (self self (car divrem) (cons (+ 48 (cdr divrem)) tail)) )
                   ((n divrem-update) (k 0)) )
                  tail )))))

(lazy-def '(divrem-update prev)
 '(if (car (((cdr prev) cdr) ((9 (cons #f)) (k #t))))
      (cons (1+ (car prev)) 0)
      (cons (car prev) (1+ (cdr prev))) ))


(lazy-def 'fibs
 '((lambda (x) (x x 2 1))
   (lambda (self a b)
     (cons a (self self b (+ a b))))))

(lazy-def 'nth-fib
  '(car (cdr (cdr fibs))))

(print-as-cc (laze 'nth-fib))
;(print-as-cc (laze 'print-fibs))
