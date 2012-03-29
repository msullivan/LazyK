signature THUNK =
sig
  type 'a thunk
  val force : 'a thunk -> 'a
  val delay : (unit -> 'a) -> 'a thunk
  val no_delay : (unit -> 'a) -> 'a thunk
end

structure Thunk :> THUNK =
struct
  type 'a thunk = (unit -> 'a) ref
  fun force (ref f) = f ()
  (* Make a non updating thunk *)
  fun no_delay f = ref f

  fun delay f =
      let val r = ref (fn () => raise Fail "hono")
          fun thunk () =
              let val x = f ()
                  val () = r := (fn () => x)
              in x end
          val () = r := thunk
      in r end
end

structure LazyK =
struct
  val % = Thunk.delay
  val %% = Thunk.no_delay

  datatype comb' = Func of comb -> comb
                 | Num of int 
  withtype comb = comb' Thunk.thunk

  infix $$
  fun f $$ g =
      let val (Func f') = Thunk.force f
      in f' g end (* XXX: more lazy? *)
  fun getNum f =
      let val (Num n) = Thunk.force f
      in n end

  (* Implementation of the combinators *)
  val I : comb = % (fn () => Func (fn x => x))
  val K : comb = % (fn () => Func (
                             fn x => % (fn () => Func (fn _ => x))))
  val S : comb = % (fn () => Func (fn x =>
                  % (fn () => Func (fn y =>
                   % (fn () => Func (fn z =>
                                        (x $$ z) $$ (y $$ z)))))))
  (* The hacky bullshit combinator - used to extract useful numbers from church numerals *)
  val inc : comb = % (fn () => Func (fn x => 
                    % (fn () => Num (getNum x + 1))))
  val zero : comb = % (fn () => Num 0)


  (* Useful functions for constructing and destructing combinators *)
  fun car e = e $$ K
  fun cdr e = e $$ (K $$ I)
  fun cons x xs = S $$ (S $$ I $$ (K $$ x)) $$ (K $$ xs)

  fun churchIncrement c = S $$ (S $$ (K $$ S) $$ K) $$ c
  fun fromChurchNumeral c = getNum (c $$ inc $$ zero)

  fun iterate 0 _ x = []
    | iterate n f x = x :: iterate (n-1) f (f x)

  val churchNumeralTable = Vector.fromList (iterate 257 churchIncrement (K $$ I))
  fun getChurchNumeral n =
      if n < 0 orelse n > 256 then Vector.sub (churchNumeralTable, 256)
      else Vector.sub (churchNumeralTable, n)
end
