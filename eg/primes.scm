(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")


(lazy-def '(print-primes input)
 '(digitize-list (sieve 2 (list-of #f))) )


(lazy-def 'sieve
 '((lambda (x) (x x))
   (lambda (self)
     (lambda (n composites)
       ((if (car composites) i (cons n))
        (self self
              (1+ n)
              ((if (car composites)
                   i
                   (map2 or (true-every n)) )
               (cdr composites) )))))))

(lazy-def '(true-every n)
 '((lambda (x) (x x))
   (lambda (self)
     (cdr ((n (cons #f)) (cons #t (self self)))) )))

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
                   (cons 32 (self self (cdr lst))) )))))

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


(print-as-cc (laze 'print-primes))
