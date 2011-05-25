;; Brainfuck in Lazy K.
;; Copyright 2002 Ben Rudiak-Gould. Distributed under the GPL.
;;
;; This is not a Brainfuck interpreter, but a direct translation of
;; Brainfuck instructions into Lazy K code, just as
;;
;;       +     ++*p;
;;       [     while (*p) {
;;
;; and so on is a direct translation of Brainfuck instructions into
;; C code. A Brainfuck program such as .[.+] is equivalent to
;; (bf-prolog (bf-write (bf-begin (bf-write (bf-inc (bf-end bf-epilog)))))).
;; The enclosed bf2lazy.c program performs this translation automatically.
;;
;; Implementation details:
;;
;; The functions communicate among themselves to obtain references to
;; whichever nearby instructions they need. In the case of [ this is the
;; following instruction and the instruction after the matching ]; in the
;; case of ] it is the following instruction and the matching [; in all
;; other cases it is the following instruction only. References to later
;; functions are passed to the left as return values, and references to
;; earlier functions are passed to the right as arguments. The bf-prolog
;; guard function creates a blank memory store and passes it to the first
;; real instruction. The bf-epilog function appends end-of-file to the
;; output, which causes Lazy K execution to terminate.
;;
;; The Brainfuck memory store is unbounded in extent in both directions from
;; the origin, and is initialized to all zeroes. It is represented as
;; (cur,left,right), where cur holds the current cell and left and right are
;; (infinite) lists of the cells to the left and right of the current cell,
;; nearest first. Each memory cell holds an 8-bit value, implemented in a
;; similar way as a pair of lists of #f with #t guards on either end,
;; representing 0. Supporting unbounded integer values would have been
;; easier; however, I discovered that many existing Brainfuck programs rely
;; on [-] zeroing a cell even when its value has been decremented below
;; zero, and this, together with the reasonable requirement that -+ be
;; always a no-op, forces the use of modular arithmetic.
;;
;; The length of translated programs could be substantially reduced by
;; moving most of the logic of the Brainfuck operations into bf-prolog and
;; turning bf-inc, etc. into simple selector functions, but that wouldn't be
;; as much fun.
;;
;; In many cases certain function arguments below are implicit, because I
;; optimized the functions by changing (lambda (x) (f x)) to f (thus hiding
;; the existence of x in this example). This isn't really necessary, since
;; Lazier performs this optimization itself, but it's fun.
;;
;; I haven't checked to see what will happen if the brackets aren't
;; balanced, or if the Brainfuck programs tries to read past end-of-file; it
;; may not be pretty.
;;
;; Unfortunately, every Brainfuck program I've run under Lazy K leaks
;; memory, even if its resource usage in Brainfuck space is constant. I
;; haven't yet discovered why this happens.
;;
;;
;; a top-level function (bf-*):
;;  - is passed a list of (second-level func for next instr., second-level func for after next ], ...)
;;  - returns an updated version of same
;;
;; a second-level function:
;;  is passed third-level func for the previous [ test
;;  returns a third-level func for self
;;
;; a third-level function:
;;  is passed cur, left, right, input
;;  returns output
;;


(load "../lazier.scm")
(load "../prelude.scm")


(lazy-def 'bf-begin		; [
 '(lambda (fwd-list)
    (cons (lambda (back)
            ((lambda (x) (x x))
             (lambda (self)
               (lambda (cur)
                 ((if (car (car cur))
                      ((car (cdr fwd-list)) back)
                      ((car fwd-list) (self self)) )
                  cur )))))
          (cdr (cdr fwd-list)) )))


(lazy-def 'bf-end		; ]
 '(lambda (fwd-list)
    (cons (lambda (back) back)
          fwd-list )))


(lazy-def 'bf-inc		; +
 '(lambda (fwd-list)
    (cons (lambda (back)
            (lambda (cur)
              ((car fwd-list)
               back
               ((lambda (a b)
                  (if (car b) (cons b a) (cons a b)) )
                (cons #f (car cur))
                (cdr (cdr cur)) ))))
          (cdr fwd-list) )))


(lazy-def 'bf-dec		; -
 '(lambda (fwd-list)
    (cons (lambda (back)
            (lambda (cur)
              ((car fwd-list)
               back
               (cur (lambda (a b)
                      (if (car a)
                          (cons (cdr b) (cons #f a))
                          (cons (cdr a) (cons #f b)) ))))))
          (cdr fwd-list) )))


(lazy-def 'bf-left		; <
 '(lambda (fwd-list)
    (cons (lambda (back)
            (lambda (cur left right)
              ((car fwd-list)
               back
               (car left)
               (cdr left)
               (cons cur right) )))
          (cdr fwd-list) )))


(lazy-def 'bf-right		; >
 '(lambda (fwd-list)
    (cons (lambda (back)
            (lambda (cur left right)
              ((car fwd-list)
               back
               (car right)
               (cons cur left)
               (cdr right) )))
          (cdr fwd-list) )))


(lazy-def 'bf-read		; ,
 '(lambda (fwd-list)
    (cons (lambda (back)
            (lambda (cur left right input)
              ((car fwd-list)
               back
               (cons (((car input) (cons #f)) (k #t))
                     (((car input) cdr) (cdr ((256 (cons #f)) (k #t)))) )
               left
               right
               (cdr input) )))
          (cdr fwd-list) )))


(lazy-def 'bf-write		; .
 '(lambda (fwd-list)
    (cons (lambda (back)
            (lambda (cur left right input)
              (cons (count-to-true (car cur))
                    ((car fwd-list) back cur left right input) )))
          (cdr fwd-list) )))

(lazy-def 'count-to-true
 '((lambda (x) (x x))
   (lambda (self lst)
     (if (car lst) 0 (1+ (self self (cdr lst)))) )))


(lazy-def 'bf-prolog
 '((lambda (cell-list)
     (lambda (fwd-list)
       ((car fwd-list)
        i
        (car cell-list)
        cell-list
        cell-list )))
   (list-of (cons (k #t) (cdr ((256 (cons #f)) (k #t))))) ))


(lazy-def 'bf-epilog
 '(k (lambda (back)
       (lambda (cur left right input)
         end-of-output ))))
