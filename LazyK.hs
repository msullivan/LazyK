-- Lazy K interpreter in Haskell.
-- Copyright 2011 Michael Sullivan.
-- Distributed under the GPL.
-- This interpreter represents lazy k combinators as haskell
-- functions, thus taking advantage of haskell's built in support for
-- lazy evaluation. It is kind of cheating.
-- This supports all of the different syntaxes for Lazy K, but can
-- only take one source file name as input.

module LazyK
(parse,
 runFile,
 outputCharacter,
 Expr(..)
) where

import Data.Array
import Data.Char
import Data.List
import System.Exit

-- Really, every Comb should just be a function Comb -> Comb.
-- However, we need to be able to extract an Int from a church numeral,
-- so we need Nat.
data Comb = Func (Comb -> Comb)
          | Nat Int

-- Function application on a combinator - should it be more lazy?
ap :: Comb -> Comb -> Comb
ap (Func c1) = c1
($$) = ap
getNat :: Comb -> Int
getNat (Nat n) = n

---- Implementation of the combinators
i :: Comb
i = Func id
k :: Comb
k = Func (\x -> Func (\_ -> x))
s :: Comb
s = Func (\x -> Func (\y -> Func (\z -> (x $$ z) $$ (y $$ z))))
-- The hacky bullshit combinator - used to extract useful numbers from church numerals
inc :: Comb
inc = Func (\(Nat n) -> Nat (n+1))

---- Useful functions for constructing and destructing combinators
car :: Comb -> Comb
car e = e $$ k
cdr :: Comb -> Comb
cdr e = e $$ (k $$ i)
cons :: Comb -> Comb -> Comb
cons x xs = s $$ (s $$ i $$ (k $$ x)) $$ (k $$ xs)

churchIncrement :: Comb -> Comb
churchIncrement c = s $$ (s $$ (k $$ s) $$ k) $$ c
fromChurchNumeral :: Comb -> Int
fromChurchNumeral c = getNat $ c $$ inc $$ Nat 0


churchNumerals :: [Comb]
churchNumerals = iterate churchIncrement (k $$ i)

churchNumeralTable :: Array Int Comb
churchNumeralTable = array (0, 256) $ zip [0..256] churchNumerals

getChurchNumeral :: Int -> Comb
getChurchNumeral n | n < 0 || n > 256 = getChurchNumeral 256
getChurchNumeral n = churchNumeralTable ! n

-- nil doesn't really need to be anything sensible...
encodeList :: [Comb] -> Comb
encodeList = foldr cons i
decodeList :: Comb -> [Comb]
decodeList c = car c : decodeList (cdr c)

---- The core of the interpreter. The bits that drive the computation.

transformInput :: String -> Comb
transformInput l = encodeList $ map (getChurchNumeral . ord) l ++ repeat (getChurchNumeral 256)
transformOutput :: Comb -> [Int]
transformOutput = map fromChurchNumeral . decodeList

-- output a character or exit if necessary
outputCharacter :: Int -> IO ()
outputCharacter 256 = exitSuccess
outputCharacter n | n > 256 = exitWith $ ExitFailure (n-256)
outputCharacter n = putChar (chr n)

-- runComb is what drives everything
runComb :: Comb -> IO ()
runComb c = do
  --input <- getContents
  --let c' = c $$ transformInput input
  --mapM_ outputCharacter $ transformOutput c'
  putStrLn (show (fromChurchNumeral c))

---- An expression language with a conventional representation and a parser for it.
data Expr = S | K | I | App Expr Expr
          deriving Show

exprToComb :: Expr -> Comb
exprToComb S = s
exprToComb K = k
exprToComb I = i
exprToComb (App e1 e2) = exprToComb e1 $$ exprToComb e2

iota :: Expr
iota = S `App` (S `App` I `App` (K `App` S)) `App` (K `App` K)

parseJotExp :: Expr -> String -> (Expr, String)
parseJotExp e ('0':rest) = parseJotExp (e `App` S `App` K) rest
parseJotExp e ('1':rest) = parseJotExp (S `App` (K `App` e)) rest
parseJotExp e rest = (e, rest)

parseExp :: Bool -> String -> (Expr, String)
parseExp _ ('I':rest) = (I, rest)
parseExp True ('i':rest) = (iota, rest)
parseExp False ('i':rest) = (I, rest)
parseExp _ ('K':rest) = (K, rest)
parseExp _ ('k':rest) = (K, rest)
parseExp _ ('S':rest) = (S, rest)
parseExp _ ('s':rest) = (S, rest)
parseExp _ ('`':rest) = parseApp False rest
parseExp _ ('*':rest) = parseApp True rest
parseExp _ ('(':rest) = parseCCExp True rest
parseExp _ rest@('0':_) = parseJotExp I rest
parseExp _ rest@('1':_) = parseJotExp I rest
parseExp _ (')':_) = error "paren fuckup"
parseExp _ s = error $ "other fuckup: '" ++ s ++ "'"

parseApp :: Bool -> String -> (Expr, String)
parseApp is_iota rest =
  let (e1, rest') = parseExp is_iota rest
      (e2, rest'') = parseExp is_iota rest'
  in (App e1 e2, rest'')

-- collecteAdjacent nested string
collectAdjacent :: Bool -> String -> ([Expr], String)
collectAdjacent True (')':rest) = ([], rest)
collectAdjacent False (')':_) = error "unmatched closed paren"
collectAdjacent False [] = ([], [])
collectAdjacent True [] = error "unexpected EOF"
collectAdjacent nested string =
  let (exp, rest) = parseExp False string
      (exps, rest') = collectAdjacent nested rest
  in (exp:exps, rest')

parseCCExp :: Bool -> String -> (Expr, String)
parseCCExp nested string =
  case collectAdjacent nested string of
    ([], rest) -> (I, rest)
    (exps, rest) -> (foldl1 App exps, rest)

-- Strip out whitespace and comment lines
stripNonsense :: String -> String
stripNonsense = filter (not . isSpace) . unlines . filter (not . isPrefixOf "#") . lines

parse :: String -> Expr
parse = fst . parseCCExp False . stripNonsense

-- Utilities for running programs.

runString :: String -> IO ()
runString = runComb . exprToComb . parse

runFile :: String -> IO ()
runFile sourcePath = readFile sourcePath >>= runString
