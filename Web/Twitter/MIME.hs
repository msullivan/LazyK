module Web.Twitter.MIME where

import System.IO
import System.Random
import Numeric ( showHex )
import Data.List ( intercalate )

import Codec.MIME.Type as MIME

uploadFileType :: String -> MIME.Type
uploadFileType bou = MIME.Type
  { mimeType = Multipart FormData
  , mimeParams = [("boundary", bou)]
  }

mixedType :: IO (MIMEValue, String)
mixedType = do
  let low = (2^(32::Integer)-1) :: Integer
  x <- randomRIO (low,low*low)
  let boundary = replicate 30 '-' ++ showHex x ""
  return (nullMIMEValue
    { mime_val_type = MIME.Type { mimeType   = Multipart Mixed
                                , mimeParams = [("boundary", boundary)]
				}
    }, boundary)

uploadFile :: String -> FilePath -> IO MIMEValue
uploadFile nm fp = do
{-
  let low = (2^(32::Integer)-1) :: Integer
  x <- randomRIO (low,low*low)
  let boundary = replicate 30 '-' ++ showHex x ""
-}
  let file_disp = 
        Disposition
               { dispType   = DispFormData
	       , dispParams = [ Name nm, Filename fp ]
	       }
  h <- openBinaryFile fp ReadMode
  ls <- hGetContents h
  let fileValue = 
       nullMIMEValue
        { mime_val_type = Type{mimeType=Text "plain", mimeParams=[]}
        , mime_val_disp = Just file_disp
        , mime_val_content = Single ls
        , mime_val_headers = [ ("Content-Transfer-Encoding", "binary")
	                     , ("Content-Length", show (length ls))
			     ]
        , mime_val_inc_type = True
        }
  return fileValue -- MIMEValue
{-
    { mime_val_type = uploadFileType boundary
    , mime_val_disp = Nothing
    , mime_val_content = Multi [fileValue]
    }
-}
showMIMEValue :: String -> MIMEValue -> ([(String,String)], String)
showMIMEValue m mv = 
 let marker = 
       case mimeType (mime_val_type mv) of
         Multipart{} -> 
	   case lookup "boundary" (mimeParams (mime_val_type mv)) of
	     Just x -> crnl ++ '-':'-':x
	     _ -> m
         _ -> m
 in
 ( withType $ withDisp (mime_val_headers mv)
 , (if True || null m then (crnl++) else (\x -> m ++ crnl ++ x))
    (showMIMEContent marker (mime_val_content mv))
 )
 where
  withType 
   | mime_val_inc_type mv = (("Content-Type", showType (mime_val_type mv)):)
   | otherwise = id

  withDisp = 
    case mime_val_disp mv of
      Nothing -> id
      Just d  -> (("Content-Disposition", showDisposition d):)

showMIMEContent :: String -> MIMEContent -> String
showMIMEContent _marker (Single s) = s
showMIMEContent  marker (Multi ms) = 
  concat (map (s.(showMIMEValue marker)) ms) ++ marker ++ "--"
 where
  s (hs,v) = marker ++ crnl ++
    intercalate crnl (map (\ (a,b) -> (a ++ ':':' ':b)) hs) ++ crnl ++ v

crnl :: String
crnl = "\r\n"

showDisposition :: Disposition -> String
showDisposition d = 
  showDispType (dispType d) ++
   (concat $ map showDispParam (dispParams d))
 
showDispType :: DispType -> String
showDispType dt = 
 case dt of
   DispInline -> "inline"
   DispAttachment -> "attachment"
   DispFormData   -> "form-data"
   DispOther x    -> x

showDispParam :: DispParam -> String
showDispParam dp = ';':' ':
  case dp of
    Name x -> "name="++show x
    Filename x -> "filename=" ++ show x
    CreationDate s -> "creation-date=" ++ show s
    ModDate s -> "modification-date=" ++ show s
    ReadDate s -> "read-date=" ++ show s
    MIME.Size x -> "size=" ++ show x
    OtherParam a b -> a ++ '=':show b

    
