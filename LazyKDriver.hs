module Main
(main
) where

import LazyK
import System.Environment(getArgs)

main :: IO ()
main = do
  [sourcePath] <- getArgs
  runFile sourcePath
