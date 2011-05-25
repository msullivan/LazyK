;; Some commonly useful macros for Lazier.
;;
;; There must not be any circular definitions here
;; (e.g. a definition containing its own name). If
;; you need recursion, you'll have to roll your own.
;;
;; Unfortunately, sometimes these functions have
;; more than one form, and the one which is best for
;; inlining is longer when not inlined. I know that
;; this is the case for and, or, and list-of. In all
;; three cases I picked the form preferable for
;; inlining.

;;; logic
(lazy-def #t		'(lambda (x y) x))
(lazy-def #f		'(lambda (x y) y))
(lazy-def '(if p x y)	'(p x y))
(lazy-def '(not p)	'(if p #f #t))
(lazy-def '(and p q)	'(if p q #f))
(lazy-def '(or p q)	'(if p #t q))

;;; lists
(lazy-def '(cons x y)	'(lambda (f) (f x y)))
(lazy-def '(car list)	'(list #t))
(lazy-def '(cdr list)	'(list #f))
(lazy-def ()		'(lambda (f) #t))
(lazy-def '(null? list)	'(list (lambda (x y) #f)))
(lazy-def '(nth n lst)	'(car ((n cdr) lst)))
(lazy-def '(list-ref lst n)	'(nth n lst))

;;; Church numerals (see also prelude-numbers.scm)
(lazy-def '(1+ a)	'(lambda (f) (lambda (x) (f ((a f) x)))))
(lazy-def 'succ		'1+)
(lazy-def 0		'(lambda (f) (lambda (x) x)))
(lazy-def 1		'(lambda (f) f))
(lazy-def 2		'(1+ 1))
(lazy-def 4		'((lambda (x) (x x)) 2))
(lazy-def 256		'((lambda (x) (x x)) 4))
(lazy-def '(+ a)	'(a 1+))
(lazy-def '(* a b)	'(lambda (f) (a (b f))))
(lazy-def '(^ a b)	'(b a))
(lazy-def '(ifnonzero n x y)	'((n (k x)) y))

(lazy-def '(= a b)
 '(nth b ((a (cons #f))
          (cons #t (list-of #f)) )))
(lazy-def '(!= a b)
 '(nth b ((a (cons #t))
          (cons #f (list-of #t)) )))

; the following neat <= function is stolen from the Unlambda page
(lazy-def '(if<= m n x y)	'((m ^ (k x)) (n ^ (k y))))
(lazy-def '(if> m n x y)	'(if<= m n y x))
(lazy-def '(if>= m n x y)	'(if<= n m x y))
(lazy-def '(if< m n x y)	'(if<= n m y x))


;;; miscellaneous

(lazy-def 'end-of-output '(lambda (f) 256))

(lazy-def 'list-from
 '((lambda (x) (x x))
   (lambda (self n)
     (cons n (self self (1+ n))) )))

; functional composition (same as *)
(lazy-def '(o f g)	'(lambda (x) (f (g x))))

(lazy-def '(list-of elt)
 '(Y (cons elt)) )

(lazy-def '(Y f)
 '((lambda (x) (x x))
   (lambda (self)
     (f (self self)) )))

; The following definition of the Y combinator is taken from
; "Kolmogorov Complexity in Combinatory Logic" by John Tromp
; (http://www.cwi.nl/~tromp/cl/CL.ps). It is shorter by itself,
; but longer when applied. Interestingly, there is no way (that
; I found) to produce this from a lambda expression using
; Lazier. Specifically, Lazier can't figure out that
; (lambda (y) (x (w y x))) is equivalent to SS(Sw)(Kx).

;(lazy-def 'Y '(S S K (S (K (S S (S (S S K)))) K)))
