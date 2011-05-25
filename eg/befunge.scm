;; Befunge-93 in Lazy K.
;; Copyright 2002 Ben Rudiak-Gould. Distributed under the GPL.
;;
;; Input to the Befunge program begins with the 26th line of stdin, if any.
;; This means that the program must be padded with blank lines if it expects
;; to read input.
;;
;; If the PC encounters an unrecognized character, the interpreter will print
;; it out followed by "?" and abort execution.
;;
;; Limitations:
;;
;;   - The ? command is not currently implemented, due to a lack of any
;;     source of randomness in Lazy K. It is treated as illegal (i.e. you'll
;;     get "??").
;;
;;   - Calculating with large numbers requires time and memory proportional
;;     to the absolute value of the number. In other words, it's hopelessly
;;     inefficient. Luckily most Befunge programs seem to restrict themselves
;;     to small values.
;;
;;   - Division or modulus by 0 will hang the interpreter. It wouldn't be hard
;;     to make them return an error instead, but I don't anticipate anyone
;;     using this interpreter as a development platform (and if you do, you
;;     probably don't want me to make things any easier for you anyway).
;;
;;   - I didn't examine any existing implementation when writing this, so I
;;     may have missed a detail which wasn't mentioned in the spec.
;;


(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")


(lazy-def '(main input)
 '(read-lines
   (lambda (lines input-tail)
     (run-befunge (k 0) mode-normal direction-east (list-of (cons 0 #f)) lines input-tail) )
   input ))


; cont gets lines, input tail
(lazy-def '(read-lines cont input)
 '((25 read-lines-iter)
   (lambda (lines input-tail)
     (cont (lines i) input-tail) )
   i input ))

(lazy-def '(read-lines-iter next lines input)
 '(read-line (lambda (line new-input)
               (next (o lines (cons line)) new-input) )
             input ))

; cont gets line, input tail
(lazy-def '(read-line cont)
 '((lambda (x) (x x i))
   (lambda (self partial-line input)
     ((if (if>= (car input) 256
                #t
                (= 10 (car input)) )
          (cont (partial-line (list-of 32)))
          (self self (o partial-line (cons (car input)))) )
      (cdr input) ))))


(lazy-def 'run-befunge
 '((lambda (x) (x x))
   (lambda (self xy mode direction stack lines input)
     ((lambda (current-cell-contents)
        ((if (cdr mode)
             befunge-stringmode
             (if (car mode)
                 (nth current-cell-contents dispatch-table)
                 befunge-nop ))
         (lambda (success-cont current-cell-contents direction stack lines input)
           (cons current-cell-contents (cons 63 end-of-output)) )
         (lambda (mode direction stack lines input)
           (self self
                 (if (car direction)
                     (cons (car xy)				; vertical
                           (if (cdr direction)
                               (nth (cdr xy) (cons 24 (list-from 0)))	; north
                               (if>= (cdr xy) 24 0 (1+ (cdr xy))) ))	; south
                     (cons (if (cdr direction)			; horizontal
                               (nth (car xy) (cons 79 (list-from 0)))	; west
                               (if>= (car xy) 79 0 (1+ (car xy))) )	; east
                           (cdr xy) ))
                 mode direction stack lines input ))
         current-cell-contents direction stack lines input ))
      (nth (car xy) (nth (cdr xy) lines)) ))))


(lazy-def 'direction-north '(k #t))	; (#t . #t)
(lazy-def 'direction-south 'i)		; (#t . #f)
(lazy-def 'direction-east  '(k #f))	; (#f . #f)
(lazy-def 'direction-west  'not)	; (#f . #t)


(lazy-def 'mode-normal 'i)	; (#t . #f)
(lazy-def 'mode-bridge '(k #f))	; (#f . #f)
(lazy-def 'mode-string '(k #t))	; (#t . #t)


(lazy-def 'dispatch-table
 '((32 (cons i))
   (cons befunge-nop	;   32	no operation
         (cons befunge-not	; ! 33	logical not
               (cons befunge-enter-stringmode	; " 34	stringmode
                     (cons befunge-bridge	; # 35	bridge
                           (cons befunge-pop	; $ 36	pop
                                 (cons befunge-modulo	; % 37	modulo
                                       (cons befunge-input-int	; & 38	input integer
                                             ((3 (cons i))
                                              (cons befunge-multiply	; * 42	multiply
                                                    ((lambda (f) (f befunge-add-or-subtract))
                                                     (lambda (befunge-add-or-subtract)
                                                       (cons (befunge-add-or-subtract i)	; + 43	add
                                                             (cons befunge-output-char	; , 44	output character
                                                                   dispatch-table-part2 )))))))))))))))

(lazy-def 'dispatch-table-part2
 '(cons (befunge-add-or-subtract not)	; - 45	subtract
        (cons befunge-output-int	; . 46	output integer
              (cons befunge-divide	; / 47	divide
                    ((10 (cons befunge-push-digit))	; 48..57 push digit
                     (cons befunge-dup	; : 58	dup
                           (cons i
                                 (cons befunge-left	; < 60	left
                                       (cons i
                                             (cons befunge-right	; > 62	right
                                                   (cons befunge-random	; ? 63	random
                                                         (cons befunge-end	; @ 64	end
                                                               ((27 (cons i))
                                                                (cons befunge-swap	; \ 92	swap
                                                                      dispatch-table-part3 ))))))))))))))

(lazy-def 'dispatch-table-part3
 '(cons i
        (cons befunge-up	; ^ 94	up
              (cons (befunge-if #f)	; _ 95	horizontal if
                    (cons befunge-greater	; ` 96	greater
                          ((6 (cons i))
                           (cons befunge-get	; g 103	get
                                 ((8 (cons i))
                                  (cons befunge-put	; p 112	put
                                        ((5 (cons i))
                                         (cons befunge-down	; v 118	down
                                               ((5 (cons i))
                                                (cons (befunge-if #t)	; | 124	vertical if
                                                      (cons i
                                                            (cons befunge-input-char	; ~ 126	input character
                                                                  (list-of i) )))))))))))))))


(lazy-def '(befunge-stringmode error-cont cont cell-contents direction stack lines input)
 '((if (= 34 cell-contents)
       (cont mode-normal direction stack)
       (cont mode-string direction (cons (cons cell-contents #f) stack)) )
   lines input ))


(lazy-def '(befunge-nop error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal direction stack lines input) )


(lazy-def '(befunge-not error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal
        direction
        (cons (cons (^ 0 (car (car stack))) #f) (cdr stack))
        lines
        input ))


(lazy-def '(befunge-enter-stringmode error-cont cont cell-contents direction stack lines input)
 '(cont mode-string direction stack lines input) )


(lazy-def '(befunge-bridge error-cont cont cell-contents direction stack lines input)
 '(cont mode-bridge direction stack lines input) )


(lazy-def '(befunge-pop error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal direction (cdr stack) lines input) )


(lazy-def '(befunge-modulo error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal
        direction
        (cons (cons (mod (car (car (cdr stack))) (car (car stack)))
                    (cdr (car (cdr stack))) )
              (cdr (cdr stack)) )
        lines
        input ))

(lazy-def '(mod n d)
 '(nth n (mod-list d)) )

(lazy-def '(mod-list d)
 '((d mod-list-iter)
   (lambda (n list-piece)
     (Y list-piece) )
   0 i ))

(lazy-def '(mod-list-iter next n list-piece)
 '(next (1+ n) (o list-piece (cons n))) )


(lazy-def '(befunge-input-int error-cont cont cell-contents direction stack lines input)
 '(read-int (skip-whitespace input)
    (lambda (int new-input)
      (cont mode-normal direction (cons int stack) lines new-input) )))

(lazy-def 'skip-whitespace
 '((lambda (x) (x x))
   (lambda (self list)
     (if<= (car list) 32
           (self self (cdr list))
           list ))))

(lazy-def '(read-int list cont)
 '((lambda (read-unsigned-int)
     (if (= 45 (car list))
         (read-unsigned-int #t cont (cdr list))
         (read-unsigned-int #f cont list) ))
   (lambda (negative cont)
     ((lambda (x) (x x 0))
      (lambda (self running-total list)
        (if (nth (car list) ((48 (cons #f)) ((10 (cons #t)) (list-of #f))))
            (self self
                  (+ (* 10 running-total)
                     (nth (car list) ((48 (cons i)) (list-from 0))) )
                  (cdr list) )
            (cont (cons running-total negative) list) ))))))


(lazy-def '(befunge-multiply error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal
        direction
        (cons (cons (* (car (car stack))
                       (car (car (cdr stack))) )
                    ((if (cdr (car stack)) not i) (cdr (car (cdr stack)))) )
              (cdr (cdr stack)) )
        lines
        input ))


(lazy-def '(befunge-add-or-subtract sign-op error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal
        direction
        (cons
         ((lambda (second-sign)
            (if (if (cdr (car (cdr stack))) (not second-sign) second-sign)
                (cons (nth (car (car (cdr stack))) (sub-list (car (car stack))))	; subtraction
                      (if>= (car (car (cdr stack))) (car (car stack)) #f #t) )
                (cons (+ (car (car (cdr stack))) (car (car stack))) second-sign) ))	; addition
          (sign-op (cdr (car stack))) )
         (cdr (cdr stack)) )
        lines
        input ))

(lazy-def '(sub-list shift)
 '(shift prepend-next-integer (list-from 0)) )

(lazy-def '(prepend-next-integer list)
 '(cons (1+ (car list)) list) )


(lazy-def '(befunge-output-char error-cont cont cell-contents direction stack lines input)
 '(cons (car (car stack))
        (cont mode-normal direction (cdr stack) lines input) ))


(lazy-def '(befunge-output-int error-cont cont cell-contents direction stack lines input)
 '((if (cdr (car stack)) (cons 45) i)
   (prepend-number (car (car stack))
                   (cons 32
                         (cont mode-normal direction (cdr stack) lines input) ))))

(lazy-def 'prepend-number
 '((lambda (x) (x x))
   (lambda (self num list)
     ((num remdiv10-iter)
      (lambda (rem quot self num list)
        ((ifnonzero quot (self self quot) i)
         (cons (+ 48 rem) list) ))
      0 0 self num list ))))

(lazy-def '(remdiv10-iter next rem quot)
 '(if>= rem 9
        (next 0 (1+ quot))
        (next (1+ rem) quot) ))


(lazy-def '(befunge-divide error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal
        direction
        (cons (cons (/ (car (car (cdr stack))) (car (car stack)))
                    ((if (cdr (car (cdr stack))) not i) (cdr (car stack))) )
              (cdr (cdr stack)) )
        lines
        input ))

(lazy-def '(/ n d)
 '(nth n (divide-list d)) )

(lazy-def '(divide-list d)
 '((lambda (x) (x x 0))
   (lambda (self n)
     ((d (cons n)) (self self (1+ n))) )))


(lazy-def '(befunge-push-digit error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal
        direction
        (cons (cons (nth cell-contents ((48 (cons i)) (list-from 0)))
                    #f )
              stack )
        lines
        input ))


(lazy-def '(befunge-dup error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal
        direction
        (cons (car stack) stack)
        lines
        input ))


(lazy-def '(befunge-left error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal direction-west stack lines input) )


(lazy-def '(befunge-right error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal direction-east stack lines input) )


(lazy-def 'befunge-random 'i)		; no obvious way to implement this


(lazy-def '(befunge-end error-cont cont cell-contents direction stack lines input)
 'end-of-output )


(lazy-def '(befunge-swap error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal
        direction
        (cons (car (cdr stack))
              (cons (car stack)
                    (cdr (cdr stack)) ))
        lines
        input ))


(lazy-def '(befunge-up error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal direction-north stack lines input) )


(lazy-def '(befunge-if vertical error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal
        (cons vertical (ifnonzero (car (car stack)) #t #f))
        (cdr stack)
        lines
        input ))


(lazy-def '(befunge-greater error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal
        direction
        (cons (cons ((lambda (a b)
                       (if (cdr a)
                           (if (cdr b)
                               (if< (car a) (car b) 1 0)
                               0 )
                           (if (cdr b)
                               1
                               (if> (car a) (car b) 1 0) )))
                     (car (cdr stack)) (car stack) )
                    #f )
              (cdr (cdr stack)) )
        lines
        input ))


(lazy-def '(befunge-get error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal
        direction
        (cons (cons (nth (car (car (cdr stack)))
                         (nth (car (car stack)) lines) )
                    #f )
              (cdr (cdr stack)) )
        lines
        input ))


(lazy-def '(befunge-put error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal
        direction
        (cdr (cdr (cdr stack)))
        ((lambda (modify-nth)
           (modify-nth (car (car stack))
                       (lambda (old-row)
                         (modify-nth (car (car (cdr stack)))
                                     (lambda (old-value) (car (car (cdr (cdr stack)))))
                                     old-row ))
                       lines ))
         modify-nth )
        input ))

(lazy-def '(modify-nth n filter list)
 '((n modify-nth-iter)
   (lambda (front back)
     (front (cons (filter (car back)) (cdr back))) )
   i
   list ))

(lazy-def '(modify-nth-iter next front back)
 '(next (o front (cons (car back))) (cdr back)) )


(lazy-def '(befunge-down error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal direction-south stack lines input) )


(lazy-def '(befunge-input-char error-cont cont cell-contents direction stack lines input)
 '(cont mode-normal
        direction
        (cons (cons (car input) #f) stack)
        lines
        (cdr input) ))


(print-as-unlambda (laze 'main))
