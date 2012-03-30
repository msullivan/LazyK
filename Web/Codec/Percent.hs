{- |
 
  Module      :  Web.Codec.Percent
  Copyright   :  (c) 2008, Sigbjorn Finne

  Maintainer  : sof@forkIO.com

  License     : See the file LICENSE

  Status      : Coded

  Codec for de/encoding URI strings via percent encodings
 (cf. RFC 3986.)
-}
module Web.Codec.Percent where

import Data.Char ( chr, isAlphaNum, isAscii )
import Numeric   ( readHex, showHex )

getEncodedString :: String -> String
getEncodedString "" = ""
getEncodedString (x:xs) = 
  case getEncodedChar x of
    Nothing -> x : getEncodedString xs
    Just ss -> ss ++ getEncodedString xs

getDecodedString :: String -> String
getDecodedString "" = ""
getDecodedString ls@(x:xs) = 
  case getDecodedChar ls of
    Nothing -> x : getDecodedString xs
    Just (ch,xs1) -> ch : getDecodedString xs1

getEncodedChar :: Char -> Maybe String
getEncodedChar x
 | (isAlphaNum x && isAscii x) || 
   x `elem` "-_.~" = Nothing
 | xi < 0xff       = Just ('%':showHex (xi `div` 16) (showHex (xi `mod` 16) ""))
 | otherwise       = -- ToDo: import utf8 lib
   error "getEncodedChar: can only handle 8-bit chars right now."
 where
  xi :: Int
  xi = fromEnum x

getDecodedChar :: String -> Maybe (Char, String)
getDecodedChar str =
 case str of
   ""          -> Nothing
   (x:xs) 
    | x /= '%'  -> Nothing
    | otherwise -> do
       case xs of
         (b1:b2:bs) -> 
	    case readHex [b1,b2] of
	      ((v,_):_) -> Just (Data.Char.chr v, bs)
	      _ -> Nothing
	 _ -> Nothing
