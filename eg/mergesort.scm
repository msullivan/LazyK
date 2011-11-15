;; Mergesort in Lazy K.
;; Copyright 2002 Ben Rudiak-Gould. Distributed under the GPL.
;;
;; Two programs using the mergesort are included below.
;; unix-sort sorts the lines of the input file, and
;; bwt performs the Burrows-Wheeler blocksort transform.


(load "../lazier.scm")
(load "../prelude.scm")
(load "../prelude-numbers.scm")


; Repeats merge-toplevel-step until there's just a single (sorted) list left.
; Requires input in the form (("line1") ("line2") ... ("lastline") () () () ...)

(lazy-def '(mergesort <)
 '((lambda (x) (x x))
   (lambda (self line-lists)
     (if (null? (car (cdr line-lists)))
         (car line-lists)
         (self self (merge-toplevel-step < line-lists)) ))))


; Does a pairwise list merge, (approximately) halving the number of lists each time.

(lazy-def '(merge-toplevel-step <)
 '((lambda (x) (x x))
   (lambda (self line-lists)
     (cons (merge-lists < (car line-lists) (car (cdr line-lists)))
           (self self (cdr (cdr line-lists))) ))))


; Merges two previously-sorted lists by the well-known algorithm. Preference
; is given to items from the first list, so that the overall mergesort is
; stable (though it's impossible to tell with our definition of list<).

(lazy-def '(merge-lists <)
 '((lambda (x) (x x))
   (lambda (self a b)
     (if (null? a)
         b
         (if (null? b)
             a
             (if (< (car b) (car a))
                 (cons (car b) (self self a (cdr b)))
                 (cons (car a) (self self (cdr a) b)) ))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(lazy-def '(unix-sort input)
 '(join-lines (mergesort list< (split-lines input))) )


; Given "abc\ndef\n[EOF]", returns (("abc") ("def") () () () ...)

(lazy-def 'split-lines
 '((lambda (x) (x x i))
   (lambda (self line-so-far input)
     (if>= (car input) 256
           ((lambda (last-line)
              ((if (null? (last-line))		; don't include the last line if it's empty
                   i
                   (cons (cons last-line ())) )
               (list-of ()) ))
            (line-so-far ()) )
           (if (= 10 (car input))
               (cons (cons (line-so-far ()) ())
                     (self self i (cdr input)) )
               (self self (o line-so-far (cons (car input))) (cdr input)) )))))


; Compares strings byte-lexicographically.

(lazy-def 'list<
 '((lambda (x) (x x))
   (lambda (self a b)
     (if (null? b)
         #f
         (if (null? a)
             #t
             (if< (car a) (car b)
                  #t
                  (if> (car a) (car b)
                       #f
                       (self self (cdr a) (cdr b)) )))))))


; Rejoins sorted lines with intervening newlines.

(lazy-def 'join-lines
 '((lambda (x) (x x))
   (lambda (self lists)
     (if (null? lists)
         end-of-output
         (if (null? (car lists))
             (cons 10
                   (self self (cdr lists)) )
             (cons (car (car lists))
                   (self self (cons (cdr (car lists)) (cdr lists))) ))))))


; Note: there are much more efficient ways of doing the BWT. :-)

(lazy-def '(bwt input)
 '(map-tail car end-of-output (mergesort cdr-list< (make-bwt-matrix (copy-last-to-front input)))) )


(lazy-def '(copy-last-to-front list)
 '(cons (get-last list) list) )

(lazy-def 'get-last
 '((lambda (x) (x x))
   (lambda (self list)
     (if>= (car (cdr list)) 256
           (car list)
           (self self (cdr list)) ))))


(lazy-def 'make-bwt-matrix
 '((lambda (x) (x x))
   (lambda (self input)
     (if>= (car (cdr input)) 256
           (list-of ())
           (cons (cons input ())
                 (self self (cdr input)) )))))


(lazy-def '(cdr-list< a b) '(list< (cdr a) (cdr b)))


(lazy-def '(map-tail func tail)
 '((lambda (x) (x x))
   (lambda (self list)
     (if (null? list)
         tail
         (cons (func (car list))
               (self self (cdr list)) )))))


(print-as-cc (laze 'unix-sort))
(newline)
(print-as-cc (laze 'bwt))
