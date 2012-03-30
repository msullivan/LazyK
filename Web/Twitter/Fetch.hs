--------------------------------------------------------------------
-- |
-- Module    : Web.Twitter.Fetch
-- Copyright : (c) Sigbjorn Finne, 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: so-so
-- 
-- Simple GET\/de-ref of URLs; abstracting out networking backend\/package.
--
module Web.Twitter.Fetch 
       ( readContentsURL
       , readUserContentsURL
       
       , postContentsURL

       , URLString
       , AuthUser(..)
       , nullAuthUser
       , Cookie
       ) where
       
--import Network.Curl
import Network.Browser
import Network.HTTP
import Network.URI

type URLString = String

data AuthUser
 = AuthUser { authUserName :: String
            , authUserPass :: String
	    }

nullAuthUser :: AuthUser
nullAuthUser = AuthUser
            { authUserName = ""
	    , authUserPass = ""
	    }

readContentsURL :: URLString -> IO String
readContentsURL u = do
  req <- 
    case parseURI u of
      Nothing -> fail ("ill-formed URL: " ++ u)
      Just ur -> return (defaultGETRequest ur)
    -- don't like doing this, but HTTP is awfully chatty re: cookie handling..
  let nullHandler _ = return ()
  (_u, resp) <- browse $ setOutHandler nullHandler >> request req
  case rspCode resp of
    (2,_,_) -> return (rspBody resp)
    _ -> fail ("Failed reading URL " ++ show u ++ " code: " ++ show (rspCode resp))

{- Curl version:
readContentsURL :: URLString -> IO String
readContentsURL u = do
  let opts = [ CurlFollowLocation True
	     ]
  (_,xs) <- curlGetString u opts
  return xs
-}

readUserContentsURL :: Maybe AuthUser -> Bool -> Bool -> URLString -> [(String,String)] -> IO ([(String,String)], String)
readUserContentsURL mbU doRedir isHead us hdrs = do -- readContentsURL u
  let hs = 
       case parseHeaders $ map (\ (x,y) -> x++": " ++ y) (addDefaultHeaders 0 hdrs) of
         Left{} -> []
	 Right xs -> xs
  req0 <- 
    case parseURI us of
      Nothing -> fail ("ill-formed URL: " ++ us)
      Just ur -> return (defaultGETRequest ur)
    -- don't like doing this, but HTTP is awfully chatty re: cookie handling..
  let req = insertHeaderIfMissing HdrHost (authority (rqURI req0)) $
              req0{ rqMethod=if isHead then HEAD else GET
	          , rqHeaders= hs
		  , rqURI = (rqURI req0){uriScheme="",uriAuthority=Nothing}
	          }
  let nullHandler _ = return ()
  (u, resp) <- browse $ do 
     setOutHandler nullHandler
     case mbU of
       Nothing -> return ()
       Just usr -> do
         setAllowRedirects doRedir
         setAllowBasicAuth True
         setAuthorityGen (\ _ _ -> return (Just (authUserName usr,authUserPass usr)))

--                  setAllowBasicAuth True
--		  setAuthorityGen (\ _ _ -> return (Just (authUserName usr,authUserPass usr)))
{-
                  addAuthority AuthBasic{ auUsername = userName usr
                                        , auPassword = userPass usr
                                        , auRealm    = ""
                                        , auSite     = nullURI{uriPath="/"}
                                        }
-}
     request req
  case rspCode resp of
    (2,_,_) -> return (map toP (rspHeaders resp), rspBody resp)
    (3,_,_) | not doRedir -> return (map toP (rspHeaders resp), rspBody resp)
    _ -> fail ("Failed reading URL " ++ show u ++ " code: " ++ show (rspCode resp))


postContentsURL :: Maybe AuthUser
                -> URLString
		-> [(String,String)]
		-> [Cookie]
		-> String
		-> IO ([Cookie],[(String,String)], String)
postContentsURL mbU u hdrs csIn body = do
  let hs = 
       case parseHeaders $ map (\ (x,y) -> x++": " ++ y) (addDefaultHeaders  (length body) hdrs) of
         Left{} -> []
	 Right xs -> xs
  req0 <- 
    case parseURI u of
      Nothing -> fail ("ill-formed URL: " ++ u)
      Just ur -> return (defaultGETRequest ur)
  let req = insertHeaderIfMissing HdrHost (authority (rqURI req0)) $
              req0{ rqMethod=POST
                  , rqBody=body
		  , rqHeaders= hs
		  , rqURI = (rqURI req0){uriScheme="",uriAuthority=Nothing}
	          }
--  print req -- ,body)
  let nullHandler _ = return ()
  ((_,rsp),cs) <- browse $ do
     setOutHandler nullHandler
     setAllowRedirects True
     setCookies csIn
     case mbU of
       Nothing -> return ()
       Just usr -> do
         setAllowBasicAuth True
         setAuthorityGen (\ _ _ -> return (Just (authUserName usr,authUserPass usr)))
		   
     v <- request req
     ls <- getCookies
     return (v,ls)
  case rspCode rsp of
    (2,_,_) -> return (cs,map toP (rspHeaders rsp), rspBody rsp)
    x -> fail ("POST failed - code: " ++ show x ++ ", URL: " ++ u ++ show (rspBody rsp))

toP (Header k v) = (show k, v)

addDefaultHeaders :: Int -> [(String,String)] -> [(String,String)]
addDefaultHeaders clen hs = 
  addIfMiss "User-Agent" "hs-twitter" $
  addIfMiss "Content-Length"  (show clen) hs
 where
  addIfMiss f v xs = maybe ((f,v):xs) (const xs) (lookup f xs)

{- Curl versions:
readUserContentsURL :: User -> URLString -> IO String
readUserContentsURL u url = do
  let opts = [ CurlHttpAuth [HttpAuthAny]
             , CurlUserPwd (authUserName u ++ 
	                    case userPass u of {"" -> ""; p -> ':':p })
             , CurlFollowLocation True
	     ] 
  (_,xs) <- curlGetString url opts
  return xs
     
postContentsURL :: URLString -> [(String,String)] -> String -> IO String
postContentsURL u hdrs body = do
  let opts = [ CurlCustomRequest "POST"
             , CurlFollowLocation True
	     , CurlPost True
	     , CurlPostFields [body]
	     , CurlHttpTransferDecoding False
	     ] ++ [CurlHttpHeaders (map ( \ (x,y) -> (x ++ ':':y)) hdrs)]
  rsp <- curlGetResponse u opts
  case respStatus rsp `div` 100 of
    2 -> return (respBody rsp)
    x -> fail ("POST failed - code: " ++ show x ++ ", URL: " ++ u)

-}
