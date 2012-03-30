{-
 Codec for de/encoding form data shipped in URL query strings
 or in POST request bodies. (application/x-www-form-urlencoded)
 (cf. RFC 3986.)
-}
module Web.Codec.URLEncoder 
       ( encodeString
       , decodeString

       , isUTF8Encoded
       , utf8Encode
       ) where

import qualified Codec.Binary.UTF8.String as UTF8 ( encodeString )
import Web.Codec.Percent ( getEncodedChar, getDecodedChar )

-- for isUTF8Encoded
import Data.Bits
import Data.Word ( Word32 )

utf8Encode :: String -> String
utf8Encode str
 | isUTF8Encoded str = str
 | otherwise         = UTF8.encodeString str

encodeString :: String -> String
encodeString str = go (utf8Encode str)
 where
  go "" = ""
  go (' ':xs) = '+':go xs
  go ('\r':'\n':xs) = '%':'0':'D':'%':'0':'A':go xs
  go ('\r':xs) = go ('\r':'\n':xs)
  go ('\n':xs) = go ('\r':'\n':xs)
  go (x:xs) = 
    case getEncodedChar x of
      Nothing -> x : go xs
      Just ss -> ss ++ go xs
      
decodeString :: String -> String
decodeString "" = ""
decodeString ('+':xs) = ' ':decodeString xs
decodeString ls@(x:xs) = 
  case getDecodedChar ls of
    Nothing -> x : decodeString xs
    Just (ch,xs1) -> ch : decodeString xs1


-- | @isUTF8Encoded str@ tries to recognize input string as being in UTF-8 form.
-- Will soon migrate to @utf8-string@.
isUTF8Encoded :: String -> Bool
isUTF8Encoded [] = True
isUTF8Encoded (x:xs) = 
  case ox of
    _ | ox < 0x80  -> isUTF8Encoded xs
      | ox > 0xff  -> False
      | ox < 0xc0  -> False
      | ox < 0xe0  -> check1
      | ox < 0xf0  -> check_byte 2 0xf 0
      | ox < 0xf8  -> check_byte 3 0x7  0x10000
      | ox < 0xfc  -> check_byte 4 0x3  0x200000
      | ox < 0xfe  -> check_byte 5 0x1  0x4000000
      | otherwise  -> False
 where
   ox = toW32 x
   
   toW32 :: Char -> Word32
   toW32 ch = fromIntegral (fromEnum ch)

   check1 = 
    case xs of
     [] -> False
     c1 : ds 
      | oc .&. 0xc0 /= 0x80 || d < 0x000080 -> False
      | otherwise -> isUTF8Encoded ds
      where
       oc = toW32 c1
       d = ((ox .&. 0x1f) `shiftL` 6) .|.  (oc .&. 0x3f)

   check_byte :: Int -> Word32 -> Word32 -> Bool
   check_byte i mask overlong = aux i xs (ox .&. mask)
      where
        aux 0 rs acc
         | overlong <= acc && 
	   acc <= 0x10ffff &&
           (acc < 0xd800 || 0xdfff < acc) &&
           (acc < 0xfffe || 0xffff < acc) = isUTF8Encoded rs
         | otherwise = False

        aux n (r:rs) acc
         | toW32 r .&. 0xc0 == 0x80 = 
	    aux (n-1) rs  (acc `shiftL` 6 .|. (toW32 r .&. 0x3f))

        aux _ _  _ = False

