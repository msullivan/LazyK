module Main
(main
) where

import LazyK
import System(getArgs)

main :: IO ()
main = do
  [sourcePath] <- getArgs
  runFile sourcePath
