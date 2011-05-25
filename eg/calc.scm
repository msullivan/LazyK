;; Arbitrary-precision calculator.
;; Copyright 2002 Ben Rudiak-Gould. Distributed under the GPL.


(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")


(lazy-def 'main '(o mainloop strip-whitespace))


(lazy-def 'strip-whitespace
 '((lambda (x) (x x))
   (lambda (self input)
     ((if (if<= (car input) 32 (= (car input) 10) #t)
          (cons (car input))
          i )
      (self self (cdr input)) ))))


(lazy-def '(output-number numlen num)
 '((numlen output-number-iter)
   (lambda (output strip-leading-zeroes num-tail)
     (o strip-leading-zeroes output) )
   i i num ))

(lazy-def '(output-number-iter next output strip-leading-zeroes num)
 '(next (o (cons (+ 48 (car num))) output)
        (ifnonzero (car num) i (o cdr strip-leading-zeroes))
        (cdr num) ))


(lazy-def 'mainloop
 '((lambda (x) (x x))
   (lambda (self input)
     (if>= (car input) 256
           end-of-output
           (parse input
                  (lambda (input-tail num numlen)
                    (output-number
                     numlen
                     num
                     (cons 10
                           (self self (cdr input-tail)) ))))))))


(lazy-def 'parse
 '((lambda (x) (x x))
   (lambda (self input cont)
     (parse+ (self self) input cont) )))


(lazy-def '(parse+ parse)
 '((lambda (x) (x x))
   (lambda (self input cont)
     ((lambda (remdiv10-list)
        ((lambda (list+)
           (parse* parse list+ remdiv10-list
                   input
                   (lambda (input2 a alen)
                     (if (= 43 (car input2))		; +
                         (self self
                               (cdr input2)
                               (lambda (input3 b blen)
                                 (cont input3 (list+ a b 0) (+ alen blen)) ))
                         (cont input2 a alen) ))))
         (list+ remdiv10-list) ))
      remdiv10-list ))))


(lazy-def '(parse* parse list+ remdiv10-list)
 '((lambda (x) (x x))
   (lambda (self input cont)
     ((lambda (list*)
        (parse^ parse list*
                input
                (lambda (input2 a alen)
                  (if (= 42 (car input2))		; *
                      (self self
                            (cdr input2)
                            (lambda (input3 b blen)
                              (cont input3 (list* a b) (+ alen blen)) ))
                      (cont input2 a alen) ))))
      (list* remdiv10-list list+) ))))


(lazy-def '(parse^ parse list*)
 '((lambda (x) (x x))
   (lambda (self input cont)
     (parse_ parse
             input
             (lambda (input2 a alen)
               (if (= 94 (car input2))			; ^
                   (self self
                         (cdr input2)
                         (lambda (input3 b blen)
                           ((lambda (b-as-church)
                              (cont input3 (list^ list* a b-as-church) (1+ (* alen b-as-church))) )
                            (list->church b blen) )))
                   (cont input2 a alen) ))))))


(lazy-def '(parse_ parse input cont)
 '(if (= 40 (car input))				; (
      (parse (cdr input)
             (lambda (input2 num numlen)
               (if (= 41 (car input2))
                   (cont (cdr input2) num numlen)
                   (cons 63
                         end-of-output ))))
      ((lambda (x) (x x input (list-of 0) 0))
       (lambda (self input num numlen)
         (if (ascii-digit? (car input))			; 0..9
             (self self
                   (cdr input)
                   (cons (- (car input) 48) num)
                   (1+ numlen) )
             (cont input num numlen) )))))

(lazy-def '(ascii-digit? n)
 '(nth n ((48 (cons #f)) ((10 (cons #t)) (list-of #f)))) )

(lazy-def '(- a b)
 '(nth a ((b (cons 0)) (list-from 0))) )


(lazy-def 'remdiv10-list
 '((lambda (f) (f (cons 0 0) ((9 (cons #f)) (k #t))))
   ((lambda (x) (x x))
    (lambda (self cell left)
      (cons cell
            (if (car left)
                (self self (cons 0 (1+ (cdr cell)))
                           ((9 (cons #f)) (k #t)) )
                (self self (cons (1+ (car cell)) (cdr cell))
                           (cdr left) )))))))


(lazy-def '(list+ remdiv10-list)
 '((lambda (x) (x x))
   (lambda (self a b carry)
     ((lambda (sum)
        (cons (car sum)
              (self self (cdr a) (cdr b) (cdr sum)) ))
      (nth (+ carry (+ (car a) (car b))) remdiv10-list) ))))


(lazy-def '(list* remdiv10-list list+ a)
 '((lambda (x) (x x))
   (lambda (self b)
     (list+ (list*1 remdiv10-list (car b) a 0)
            (cons 0 (self self (cdr b)))
            0 ))))

(lazy-def '(list*1 remdiv10-list a)
 '((lambda (x) (x x))
   (lambda (self b carry)
     ((lambda (product)
        (cons (car product)
              (self self (cdr b) (cdr product)) ))
      (nth (+ carry (* a (car b))) remdiv10-list) ))))


(lazy-def '(list^ list* base)
 '((lambda (x) (x x))
   (lambda (self exp)
     (ifnonzero exp
       (halve exp
         (lambda (rem quot)
           ((rem (list* base))
            ((lambda (n) (list* n n))
             (self self quot) ))))
       (cons 1 (list-of 0)) ))))


(lazy-def '(list->church list len)
 '((len list->church-iter)
   (lambda (list n place) n)
   list 0 1 ))

(lazy-def '(list->church-iter next list n place)
 '(next (cdr list)
        (+ n (* place (car list)))
        (* 10 place) ))


(lazy-def '(halve n cont)
 '((n halve-iter)
   cont
   0 0 ))

(lazy-def '(halve-iter next rem quot)
 '(next (^ 0 rem) (+ rem quot)) )


(print-as-unlambda (laze 'main))
