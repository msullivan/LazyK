module Web.Twitter.Post where

import Codec.MIME.Type as MIME
import Codec.MIME.Parse as MIME
import Web.Twitter.MIME
import Web.Codec.URLEncoder

import Data.List
import System.Random
import Numeric

-- ease the working with POST requests and their
-- outgoing payloads.

data PostReq
 = PostReq
    { prName :: String
    , prVals :: [PostParam]
    }

data PostKind
 = PostQuery
 | PostWWWForm
 | PostFormData

newPostRequest :: String -> PostReq
newPostRequest s = PostReq{prName=s,prVals=[]}

testRequest :: PostReq
            -> Maybe PostKind
	    -> IO ()
testRequest a b = do
  (as,bs,cs) <- toRequest a b
  putStrLn ("URL query portion: " ++ as)
  putStrLn (unlines $ map (\ (k,v) -> k ++ ':':' ':v) bs)
  putStrLn ""
  putStrLn cs
  

toRequest :: PostReq
          -> Maybe PostKind
	  -> IO (String, [(String,String)], String)
toRequest pr mbKind = 
  case mbKind of
    Nothing -> 
      case filter isPostFile (prVals pr) of
        (_:_) -> toRequest pr (Just PostFormData)
	_ -> toRequest pr (Just PostWWWForm)
    Just PostQuery -> 
      case partition isPostFile (prVals pr) of
        (ls@(_:_),bs) -> do
	  putStrLn ("toRequest: POST request contains " ++ 
	        shows (length ls) (" files; unable to represent as query string"))
          putStrLn ("Defaulting to multiform/form-data instead")
	  toRequest pr{prVals=bs++ls} (Just PostFormData)
        _ -> 
	  let
	   (body_enc, xs) = partition mustBeBody (prVals pr)
	   body 
	    | null body_enc = ""
	    | otherwise     = toAmpString body_enc
	  in
	  return ( toAmpString xs
	         , ("Content-Length", show (length body)) :
		   if null body_enc 
		    then []
		    else [ ("Content-Type", "application/x-www-form-urlencoded") ]
	         , body
		 )
    Just PostWWWForm ->
      case partition isPostFile (prVals pr) of
        (ls@(_:_),bs) -> do
	  putStrLn ("toRequest: POST request contains " ++ 
	        shows (length ls) (" files; unable to represent as application/x-www-form-urlencoded"))
          putStrLn ("Defaulting to multiform/form-data instead")
	  toRequest pr{prVals=bs++ls} (Just PostFormData)
        _ -> do
	  let
	   (qs, xs) = partition mustBeQuery (prVals pr)
	   body     = toAmpString xs
	  return ( toAmpString qs
	         , [ ("Content-Type", "application/x-www-form-urlencoded")
		   , ("Content-Length", show (length body))
		   ]
	         , body
		 )
    Just PostFormData -> do
      let (qs, xs) = partition mustBeQuery (prVals pr)
      mv <- toMIMEValue xs
      let (hs,bod) = showMIMEValue "" mv
      putStrLn bod
      return ( toAmpString qs
             , ("Content-Length", show (length bod)):hs
	     , bod
	     )

toAmpString :: [PostParam] -> String
toAmpString xs = 
  intercalate "&" $ 
    map (\ (PostNameValue n v _) -> encodeString n ++ '=':encodeString v) xs

mustBeBody :: PostParam -> Bool
mustBeBody (PostNameValue _ _ (Just True)) = True
mustBeBody _ = False

mustBeQuery :: PostParam -> Bool
mustBeQuery (PostNameValue _ _ (Just False)) = True
mustBeQuery _ = False

-- | @addNameValue nm val req@ augments the request @req@ with a binding
-- for @(nm,val)@. Neither @nm@ nor @val@ are assumed encoded. It leaves it
-- until the serialization phase to fix on how to communicate the binding
-- for the POST request (i.e., via the query portion or in the request's body.)
addNameValue :: String -> String -> PostReq -> PostReq
addNameValue n v pr = pr{prVals=(PostNameValue n v Nothing):prVals pr}

-- | @addQueryNameValue nm val req@ performs same function as @addNameValue@,
-- but adds the constraint that the binding must be transmitted as part of the query
-- portion of the URL it ends up going out via.
addQueryNameValue :: String -> String -> PostReq -> PostReq
addQueryNameValue n v pr = pr{prVals=(PostNameValue n v (Just False)):prVals pr}

-- | @addQueryNameValue nm val req@ performs same function as @addNameValue@,
-- but adds the constraint that the binding must be transmitted as part of the
-- body of the POST request, forcing the payload to be of MIME type @application/x-www-form-urlencoded@
addBodyNameValue :: String -> String -> PostReq -> PostReq
addBodyNameValue n v pr = pr{prVals=(PostNameValue n v (Just True)):prVals pr}

-- | @addNameFile nm fb mbMimeType req@ augments the request @req@ with a binding
-- of name @nm@ to the local file @fb@. It will be slurped in and included in the
-- POST request, as part of a multi-part payload.
addNameFile :: String -> FilePath -> Maybe String -> PostReq -> PostReq
addNameFile nm fp mbTy pr = pr{prVals=(PostFile nm fp mbTy):prVals pr}

data PostParam
 = PostNameValue String       -- name
                 String       -- value (assume: un-encoded)
		 (Maybe Bool) -- Just True => must be in query; Just False => must be in body; Nothing => either way.
 | PostFile String         -- name
            FilePath       -- local file to post
	    (Maybe String) --  Just ty => use 'ty' as content-type

isPostFile :: PostParam -> Bool
isPostFile PostFile{} = True
isPostFile _ = False

toMIMEValue :: [PostParam] -> IO MIMEValue
toMIMEValue ps = do
  let low = (2^(32::Integer)-1) :: Integer
  x <- randomRIO (low,low*low)
  let boundary = replicate 30 '-' ++ showHex x ""
  let (fs,ns) = 
        case partition isPostFile ps of
	  ([_],_) -> ([],ps)
	  xs -> xs

  fns <- mapM (fromPostParam boundary) ns
  (mi,b)  <- mixedType
  ffs <- mapM (fromPostParam b) fs
  let addM [] = []
      addM xs = [mi{mime_val_content=Multi xs}]
      
  return MIMEValue
    { mime_val_type    = MIME.Type{ mimeType   = Multipart FormData
                                  , mimeParams = [("boundary", boundary)]
				  }
    , mime_val_disp    = Nothing
    , mime_val_content = Multi (fns ++ addM ffs)
    , mime_val_inc_type = True
    , mime_val_headers = []
    }

fromPostParam :: String -> PostParam -> IO MIMEValue
fromPostParam _boundary (PostNameValue n v _mbQ) = 
  return MIMEValue
     { mime_val_type = MIME.Type
         { mimeType = Application "x-www-form-urlencoded"
	 , mimeParams=[]
	 }
     , mime_val_disp = Just $
         Disposition { dispType = DispFormData 
                     , dispParams = [Name n]
		     }
     , mime_val_content = Single v -- (encodeString v)
     , mime_val_headers = []
     , mime_val_inc_type = False
     }
fromPostParam _boundary (PostFile nm fp mbTy) = do
  ty <- 
    case mbTy of
      Nothing -> getMIMEType fp
      Just ty -> toMIMEType ty
  mv <- uploadFile nm fp
  return mv{mime_val_type=ty}

toMIMEType :: String -> IO Type
toMIMEType tyStr = 
  case parseMIMEType tyStr of
    Just t -> return t
    _      -> return MIME.Type{mimeType=Text "plain",mimeParams=[]}

getMIMEType :: String -> IO Type
getMIMEType x = 
  case parseMIMEType x of
    Just t -> return t
    _      -> return MIME.Type{mimeType=Application "octet-stream",mimeParams=[]}
