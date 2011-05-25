;; Unlambda interpreter in Lazy K.
;; Copyright 2002 Ben Rudiak-Gould. Distributed under the GPL.
;;
;; Implementation details:
;;  - The whole interpreter is written in continuation-passing
;;    style, because there's no other way to support the c
;;    builtin. (Well, also, I have to pass the input stream
;;    around, and that's more convenient with continuations.)
;;  - In order to handle d, only the left-hand side of an
;;    application is evaluated before being applied; the right-
;;    hand side is passed to it in an unevaluated state. All
;;    functions except d evaluate their right hand side before
;;    doing anything else.
;;  - Many functions have the form (lambda (... input) (... input)),
;;    which is equivalent to (lambda (...) ...) (i.e. you can
;;    omit the input argument and make it implicit). I did this
;;    in many cases below, which was probably a bad idea since
;;    although it reduces clutter it also makes the code harder
;;    to understand.
;;
;; Unlambda language subtleties:
;;  - Sxyz has to delay the evaluation of yz, in case xz
;;    evaluates to d.
;;  - Applying `dF evaluates F every time, not just the first
;;    time (unlike Scheme's delay/force).
;;


(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")


;; expr. for eval: takes continuation & input, calls
;; (cont expr-for-apply new-input), which returns output
;;
;; expr. for apply: takes arg-for-eval & continuation & input,
;; calls (cont result-for-apply new-input), which returns output


(lazy-def 'main
 '(eval
   (lambda (program input)
     (program (lambda (program-evaluated input-tail) end-of-output)
              (cons 256 input) ))))


;; cont gets (result-for-eval new-input)
(lazy-def 'eval
 '((lambda (x) (x x))
   (lambda (self cont input)
     ((lambda (dispatch-list)
        ((nth (car input) dispatch-list) cont (cdr input)) )
      (dispatch-list
       (self self)
       (lambda (cont input)
         (cons 63 input) ))))))


;  32    (2)
;  35 # (10)
;  46 . (16)
;  63 ?
;  64 @ (31)
;  96 `  (2)
;  99 c
; 100 d
; 101 e  (3)
; 105 i  (1)
; 107 k  (6)
; 114 r
; 115 s  (2)
; 118 v  (5)
; 124 |

(lazy-def '(dispatch-list eval error)
 '((33 (cons eval))
   ((2 (cons error))
    (cons (lambda (cont)			; #
            ((lambda (x) (x x))
             (lambda (self input)
               (if (car (((car input) cdr) (true-at 10)))
                   (eval cont input)
                   (self self (cdr input)) ))))
          ((10 (cons error))
           (cons (lambda (cont input)		; .x
                   (cont (spot-eval (car input)) (cdr input)) )
                 ((16 (cons error))
                  (cons (lambda (cont input)	; ?x
                          (cont (what-eval (car input)) (cdr input)) )
                        (cons (lambda (cont) (cont whirlpool-eval))	; @
                              (cdr
                               ((32 (cons error))
                                (cons (lambda (cont)			; `
                                        (eval (lambda (lhs)
                                                (eval (lambda (rhs) (cont (appl-eval lhs rhs)))) )))
                                      ((2 (cons error))
                                       (cons (lambda (cont) (cont c-eval))
                                             (cons (lambda (cont) (cont d-eval))
                                                   (cons (lambda (cont) (cont e-eval))
                                                         ((3 (cons error))
                                                          (cons (lambda (cont) (cont i-eval))
                                                                (cons error
                                                                      (cons (lambda (cont) (cont k-eval))
                                                                            ((6 (cons error))
                                                                             (cons (lambda (cont) (cont (spot-eval 10)))
                                                                                   (cons (lambda (cont) (cont s-eval))
                                                                                         ((2 (cons error))
                                                                                          (cons (lambda (cont) (cont v-eval))
                                                                                                ((5 (cons error))
                                                                                                 (cons (lambda (cont) (cont spike-eval))
                                                                                                       (list-of error) )))))))))))))))))))))))))))


(lazy-def '(true-at n)
 '((n (cons #f)) (cons #t (list-of #f))) )


(lazy-def '(uneval expr)
 '(lambda (cont) (cont expr)) )


(lazy-def '(spot-eval char cont)
 '(cont (spot-apply char)) )

(lazy-def '(spot-apply char raw-arg cont)
 '(raw-arg
   (lambda (arg input)
     (cons char (cont arg input)) )))


(lazy-def '(what-eval char cont)
 '(cont (what-apply char)) )

(lazy-def '(what-apply char raw-arg cont)
 '(raw-arg
   (lambda (arg input)
     (arg (uneval (nth (car input)
                       ((char (cons i-apply))
                        (cons v-apply
                              (list-of i-apply) ))))
          cont input ))))


(lazy-def 'whirlpool-eval '(uneval whirlpool-apply))

(lazy-def '(whirlpool-apply raw-arg cont)
 '(raw-arg
   (lambda (arg input)
     (arg (uneval (nth (car (cdr input))
                       ((256 (cons i-apply)) (k v-apply)) ))
          cont (cdr input) ))))


(lazy-def '(appl-eval operator-eval operand-eval cont)
 '(operator-eval
   (lambda (operator-apply)
     (operator-apply operand-eval cont) )))


(lazy-def 'c-eval '(uneval c-apply))

(lazy-def '(c-apply raw-arg cont)
 '(raw-arg
   (lambda (arg)
     (arg (uneval (lambda (raw-rtn-val ignored-cont)
                    (raw-rtn-val cont) ))
          cont ))))


(lazy-def 'd-eval '(uneval d-apply))

(lazy-def '(d-apply raw-arg cont)
 '(cont (d1-apply raw-arg)) )

(lazy-def '(d1-apply raw-operator raw-operand cont)
 '(raw-operand
   (lambda (operand)
     (raw-operator
      (lambda (operator)
        (operator (uneval operand) cont) )))))


(lazy-def 'e-eval '(uneval e-apply))

(lazy-def '(e-apply raw-arg cont)
 '(raw-arg (lambda (arg input) end-of-output)) )


(lazy-def 'i-eval '(uneval i-apply))

(lazy-def '(i-apply raw-arg cont)
 '(raw-arg cont) )


(lazy-def 'k-eval '(uneval k-apply))

(lazy-def '(k-apply raw-arg1 cont)
 '(raw-arg1 (lambda (arg1)
              (cont (k1-apply arg1)) )))

(lazy-def '(k1-apply arg1 raw-arg2 cont)
 '(raw-arg2 (lambda (arg2)
              (cont arg1) )))


(lazy-def 's-eval '(uneval s-apply))

(lazy-def '(s-apply raw-arg1 cont)
 '(raw-arg1
   (lambda (arg1)
     (cont (s1-apply arg1)) )))

(lazy-def '(s1-apply arg1 raw-arg2 cont)
 '(raw-arg2
   (lambda (arg2)
     (cont (s2-apply arg1 arg2)) )))

(lazy-def '(s2-apply arg1 arg2 raw-arg3 cont)
 '(raw-arg3
   (lambda (arg3)
     (arg1 (uneval arg3)
           (lambda (xz)
             (xz (arg2 (uneval arg3))
                 cont ))))))


(lazy-def 'v-eval '(uneval v-apply))

(lazy-def 'v-apply
 '((lambda (x) (x x))
   (lambda (self raw-arg cont)
    (raw-arg (lambda (arg)
               (cont (self self)) )))))


(lazy-def 'spike-eval '(uneval spike-apply))

(lazy-def '(spike-apply raw-arg cont)
 '(raw-arg
   (lambda (arg input)
     (arg (uneval (if (nth (car input)
                           ((256 (cons #t)) (k #f)) )
                      (spot-apply (car input))
                      v-apply ))
          cont input ))))


(print-as-unlambda (laze 'main))
