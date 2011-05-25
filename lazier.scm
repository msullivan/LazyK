;;
;; Lazier, a "compiler" from lambda calculus to Lazy K.
;; Copyright 2002 Ben Rudiak-Gould. Distributed under the GPL.
;;
;; Usage examples:
;;
;; > (lazy-def '(myprog input) '(cdr (cdr input))) ; drops first two bytes of input
;; > (lazy-def 'myprog '(o cdr cdr))	; equivalent definition
;;
;; > (laze 'myprog)
;; ((s ((s i) (k (k i)))) (k (k i)))
;;
;; > (print-as-cc (laze 'myprog))
;; S(SI(K(KI)))(K(KI))
;;
;; > (print-as-unlambda (laze 'myprog))
;; ``s``si`k`ki`k`ki
;;
;; > (print-as-iota (laze 'myprog))
;; ***i*i*i*ii***i*i*i*ii*ii**i*i*ii**i*i*ii*ii**i*i*ii**i*i*ii*ii
;;
;; > (print-as-jot (laze 'myprog))
;; 111111100011111110001111111110000011110011110011111111100000111100111100
;; 11111111100000
;;
;; > (lazy-def '(f x y z) '(x (y z)))			; \
;; > (lazy-def 'f '(lambda (x y z) '(x (y z))))		; | equivalent
;; > (lazy-def 'f '(lambda (x) (lambda (y) ...)))	; /
;;
;; > (laze '(f arg1 arg2))
;; ((s (k arg1)) arg2)
;;
;; > (print-as-unlambda (laze '(f arg1 arg2)))
;; ``s`k[arg1][arg2]
;;


; lazy-def.

(define lazy-defs '())

(define (lazy-def name body)
  (set! lazy-defs
        (cons (if (pair? name)
                  (cons (car name)
                        (curry-lambda (cdr name) (curry body)) )
                  (cons name (curry body)) )
              lazy-defs )))

(define (lazy-def-lookup name)
  (assv name lazy-defs) )


; Currying.

(define (curry expr)
  (cond ((not (pair? expr)) expr)
        ((eq? (car expr) 'lambda)
         (curry-lambda (cadr expr) (curry (caddr expr))) )
        (else
         (curry-app (map curry expr)) )))

(define (curry-lambda vars body)
  (if (null? vars)
      body
      `(lambda (,(car vars)) ,(curry-lambda (cdr vars) body)) ))

(define (curry-app lst)
  (let iter ((sofar (car lst))
             (togo (cdr lst)) )
    (if (null? togo)
        sofar
        (iter (list sofar (car togo)) (cdr togo)) )))


; Macro expansion.

(define (expr-dispatch expr leaf appl lamb)
  (if (pair? expr)
      (if (eq? (car expr) 'lambda)
          (lamb (caadr expr) (caddr expr))
          (appl (car expr) (cadr expr)) )
      (leaf expr) ))

(define (expand-macros expr)
  (let helper ((expr expr) (exclude '()) (stack '()))
    (expr-dispatch expr
     (lambda (leaf)
       (cond ((memv leaf exclude) leaf)
             ((memv leaf stack)
              (display "Recursion within lazy-defs detected: ")
              (display (cons leaf stack))
              (newline)
              (error) )
             (else
              (let ((def (lazy-def-lookup leaf)))
                (if def
                    (helper (cdr def) exclude (cons leaf stack))
                    leaf )))))
     (lambda (f g)
       (list (helper f exclude stack) (helper g exclude stack)) )
     (lambda (var body)
       `(lambda (,var) ,(helper body (cons var exclude) stack)) ))))


; Replace ((lambda (var) body) value) with body[value/var] if:
;
;   - value is a symbol, or
;   - var appears only once in body and value contains no
;     more than one free variable which is not in body.
;
; I'm not sure if the first of these is ever needed -- it may
; always be handled by the other optimizations -- but it's easy
; to check for.

(define (apply-lambdas expr)
  (let ((top-level-free-vars (free-vars expr)))
    (let self ((expr expr))
      (expr-dispatch expr
       (lambda (leaf) leaf)
       (lambda (f g)
         (let ((f: (self f))
               (g: (self g)) )
           (expr-dispatch f:
            (lambda (leaf) (list f: g:))
            (lambda (f:: g::) (list f: g:))
            (lambda (var body)
              (if (or (not (pair? g:))
                      (and (<= (count-occurrences var body) 1)
                           (not (more-than-one-additional
                                 (free-vars g:)
                                 (append top-level-free-vars (free-vars f:)) ))))
                  (var-subst var g: body)
                  (list f: g:) )))))
       (lambda (var body)
         `(lambda (,var) ,(self body)) )))))

(define (add-prime var)
  (string->symbol (string-append (symbol->string var) ":")) )

(define (var-subst var value template)
  (if (eqv? var value)
      template
      (let loop ((template template))
        (expr-dispatch template
         (lambda (leaf)
           (if (eqv? var leaf) value leaf) )
         (lambda (f g)
           (list (loop f) (loop g)) )
         (lambda (v body)
           (if (eqv? var v)
               template
               (do ((template-vars (free-vars template))
                    (value-vars (free-vars value))
                    (v: v (add-prime v:)) )
                 ((and (not (memv v: template-vars))
                       (not (memv v: value-vars)) )
                  `(lambda (,v:)
                     ,(loop (var-subst v v: body)) )))))))))

(define (more-than-one-additional a b)
  (let loop ((a a) (last-sym (cons #f #f)))
    (cond ((null? a) #f)
          ((memv (car a) b)
           (loop (cdr a) last-sym) )
          ((or (pair? last-sym)		; no last symbol
               (eqv? last-sym (car a)) )
           (loop (cdr a) (car a)) )
          (else #t) )))

(define (free-vars expr)
  (let loop ((expr expr) (bound ()))
    (expr-dispatch expr
      (lambda (leaf)
        (if (memv leaf bound)
            ()
            (list leaf) ))
      (lambda (f g)
        (append (loop f bound) (loop g bound)) )
      (lambda (var body)
        (loop body (cons var bound)) ))))

(define (contains-free-variable param template)
  (expr-dispatch template
   (lambda (leaf)
     (eqv? param leaf) )
   (lambda (f g)
     (or (contains-free-variable param f)
         (contains-free-variable param g) ))
   (lambda (var body)
     (and (not (eqv? param var))
          (contains-free-variable param body) ))))

(define (count-occurrences param template)
  (expr-dispatch template
   (lambda (leaf)
     (if (eqv? param leaf) 1 0) )
   (lambda (f g)
     (+ (count-occurrences param f) (count-occurrences param g)) )
   (lambda (var body)
     (if (eqv? var param)
         0
        (count-occurrences param body) ))))


; Abstraction elimination.

(define (unabstract-lambda var body)
  (if (contains-free-variable var body)
      (expr-dispatch body
       (lambda (leaf) 'i)
       (lambda (f g)
         (if (and (eqv? var g) (not (contains-free-variable var f)))
             f
             `((s ,(unabstract-lambda var f)) ,(unabstract-lambda var g)) ))
       (lambda (v b)
         (unabstract-lambda var (unabstract body)) ))
      (list 'k body) ))

(define (unabstract code)
  (expr-dispatch code
   (lambda (leaf) leaf)
   (lambda (f g)
     (list (unabstract f) (unabstract g)) )
   (lambda (var body)
     (unabstract-lambda var (unabstract body)) )))


; Reduces expressions involving the S, K, I combinators where this
; results in a shorter expression. Usually results in only a small
; benefit.

(define (apply-ski expr)
  (if (not (pair? expr))
      expr
      (let ((lhs (apply-ski (car expr)))
            (rhs (cadr expr)) )
        (cond ((eq? lhs 'i)		; Ix -> x
               (apply-ski rhs) )
              ((and (pair? lhs)		; Kxy -> x
                    (eq? 'k (car lhs)) )
               (cadr lhs) )
              ((and (pair? lhs)		; Sxyz -> xz(yz) when x or y is K_
                    (pair? (car lhs))
                    (eq? 's (caar lhs)) )
               (let ((z rhs)
                     (y (cadr lhs))
                     (x (cadar lhs)) )
                 (if (or (and (pair? x) (eq? (car x) 'k))
                         (and (pair? y) (eq? (car y) 'k)) )
                     (apply-ski `((,x ,z) (,y ,z)))
                     (list lhs (apply-ski rhs)) )))
              (else
               (list lhs (apply-ski rhs)) )))))


; This converts expressions of the form ((x z) (y z)) to (s x y z).
; If z is just a symbol, then this change makes no difference to
; Unlambda output, always reduces the size of CC output (I think),
; and can either increase or reduce the side of Iota and Jot output.
; Currently the change is made only when z is not just a symbol.
;
; Like apply-ski, this gives only a small benefit in most cases.

(define (unapply-s expr)
  (expr-dispatch expr
   (lambda (leaf) leaf)
   (lambda (f g)
     (let ((f: (unapply-s f))
           (g: (unapply-s g)) )
       (if (and (pair? f:)
                (pair? g:)
                (pair? (cadr f:))
                (equal? (cadr f:) (cadr g:)) )
           `(((s ,(car f:)) ,(car g:)) ,(cadr f:))
           (list f: g:) )))
   (lambda (var body)
     `(lambda (,var) ,(unapply-s body)) )))


; Putting it all together.

(define (laze code)
  (unapply-s (apply-ski (unabstract (apply-lambdas (expand-macros (curry code)))))) )


; Printing it out.

(define (print-as-cc lazified-code)
  (let self ((code lazified-code))
    (expr-dispatch code
     (lambda (leaf)
      (if (memq leaf '(i k s))
          (display (char-upcase (string-ref (symbol->string leaf) 0)))
          (begin
            (display "[")
            (display leaf)
            (display "]") )))
     (lambda (f g)
      (self f)
      (if (pair? g) (display "("))
      (self g)
      (if (pair? g) (display ")")) )
     (lambda (var body)
      (error "Can't print lambdas as CC!") )))
  (newline) )


(define (print-as-generic aply k s i)
  (lambda (lazified-code)
    (let self ((code lazified-code))
      (expr-dispatch code
       (lambda (leaf)
        (cond ((eq? leaf 'i) (display i))
              ((eq? leaf 'k) (display k))
              ((eq? leaf 's) (display s))
              (else (display "[") (display leaf) (display "]")) ))
       (lambda (f g)
        (display aply)
        (self f)
        (self g) )
       (lambda (var body)
        (error "Can't print lambdas as Lazy code!") )))
    (newline) ))

(define print-as-unlambda (print-as-generic "`" "k" "s" "i"))
(define print-as-iota (print-as-generic "*" "*i*i*ii" "*i*i*i*ii" "*ii"))
(define print-as-jot (print-as-generic "1" "11100" "11111000" "11111111100000"))
