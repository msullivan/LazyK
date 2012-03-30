--------------------------------------------------------------------
-- |
-- Module      : Web.Twitter.Monad
-- Description : Monad for all things Twitter.
-- Copyright   : (c) Sigbjorn Finne, 2008-2009
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Monad for bookkeeping Twitter interactions.
-- 
--------------------------------------------------------------------
module Web.Twitter.Monad 
       ( TM
       , TMEnv(..)

       , withEnv
       , withUser
       , withCount
       , withPage
       , withPageCount
       , withAuth
       , withBase
       , withDefaultArgs
       , fromSource
       
       , getEnv
       , getUser
       , getCount
       , getPage
       , getPageCount
       , getBase
       , getPostFlag
       , getDefArgs
       , getSource
       
       , runTwitter
       , runTM
       
       , liftIO

       , api_base
       , user_base_url
       , top_base_url
       , acc_base_url
       , search_base_url
       
       , Result(..)
       , decodeStrict
       
       , mbArg
       , arg
       , strArg

       , restCall
       , postCall
       , readResult
       , postMethod
       
       ) where

import Text.JSON

import Control.Monad
import Data.List

import Web.Codec.URLEncoder
import Web.Twitter.Fetch

api_base :: URLString
api_base = "http://twitter.com/statuses/"

top_base_url :: URLString
top_base_url = "http://twitter.com/"

user_base_url :: URLString
user_base_url = "http://twitter.com/users/"

acc_base_url :: URLString
acc_base_url = "http://twitter.com/account/"

search_base_url :: URLString
search_base_url = "http://search.twitter.com/"

{-
buildUrl :: (URLString -> IO a) -> URLString -> TM a
buildUrl f u = do
  mbc <- getCount
  liftIO (f (case mbc of { Nothing -> u ; Just c ->  u++"?count="++show c}))
-}

mbArg :: String -> Maybe String -> [(String,String)] -> [(String,String)]
mbArg _ Nothing xs = xs
mbArg f (Just x) xs = (f,x):xs

arg :: String -> String -> [(String,String)] -> [(String,String)]
arg f x xs = (f,x):xs

strArg :: String -> String -> [(String,String)] -> [(String,String)]
strArg _ "" xs = xs
strArg f  x xs = (f,x):xs


restCall :: String -> [(String,String)] -> TM String
restCall u args = do
  mbc     <- getCount
  mbp     <- getPage
  d_args0 <- getDefArgs 
  mbSrc   <- getSource
  let d_args = maybe d_args0 (\ x -> ("source",x):d_args0) mbSrc
  let q = maybe id (\ x -> (("count="++show x):)) mbc $
            maybe id (\ x -> (("page="++show x):)) mbp $
	     (map (\ (x,y) -> x ++ '=':encodeString y) (args++d_args))
  b   <- getBase
  let url = b++ u ++ case q of { [] -> "" ; xs -> '?':intercalate "&" xs}
  isA <- getUser
  isP <- getPostFlag
  case isA of
    Nothing -> liftIO (readContentsURL url)
    Just au 
      | isP       -> liftIO (postContentsURL (Just au) url [] [] "" >>= \ (_,_,c) -> return c)
      | otherwise -> liftIO (readUserContentsURL (Just au) True False{-is HEAD-} url [] >>= return.snd)

postCall :: String -> [(String,String)] -> String -> [(String,String)] -> TM ([Cookie],[(String,String)], String)
postCall u hs bod args = do
  mbc <- getCount
  mbp <- getPage
  d_args0 <- getDefArgs 
  mbSrc   <- getSource
  let d_args = maybe d_args0 (\ x -> ("source",x):d_args0) mbSrc
  let q = maybe id (\ x -> (("count="++show x):)) mbc $
            maybe id (\ x -> (("page="++show x):)) mbp $
	     (map (\ (x,y) -> x ++ '=':encodeString y) (args++d_args))
  b   <- getBase
  let url = b++ u ++ case q of { [] -> u ; xs -> '?':u ++ intercalate "&" xs}
  isA <- getUser
  liftIO (postContentsURL isA url hs [] bod)

readResult :: JSON a => String -> String -> TM a 
readResult loc s = 
    case decode s of
      Ok e    -> return e
      Error e -> 
        case s of
	  ('"':xs) -> -- " strip quotes and try again..won't hurt..
	     readResult loc (init xs)
          _ -> liftIO $ ioError $ userError (loc ++ ':':' ':e)

data TMEnv
 = TMEnv
     { tmUser  :: Maybe AuthUser
     , tmBase  :: URLString
     , tmCount :: Maybe Int
     , tmPage  :: Maybe Int
     , tmPost  :: Bool
     , tmDefaultArgs :: [(String, String)]
     , tmSource :: Maybe String
     }

nullEnv :: TMEnv
nullEnv = TMEnv
  { tmUser  = Nothing
  , tmBase  = api_base
  , tmCount = Nothing
  , tmPage  = Nothing
  , tmPost  = False
  , tmDefaultArgs = []
  , tmSource = Nothing -- could be: Just "hs-twitter"
  }

newtype TM a = TM {unTM :: TMEnv -> IO a}

instance Monad TM where
  return x = TM $ \ _   -> return x
  m >>= k  = TM $ \ env -> do
     v <- unTM m env
     unTM (k v)  env

withEnv :: (TMEnv -> TMEnv) -> TM a -> TM a
withEnv fenv k = TM $ \ env -> (unTM k) (fenv env)

withUser :: AuthUser -> TM a -> TM a
withUser u k = withEnv (\ e -> e{tmUser=Just u}) k

withCount :: Int -> TM a -> TM a
withCount c k = withEnv (\e -> e{tmCount=Just c}) k

withPage :: Int -> TM a -> TM a
withPage c k = withEnv (\e -> e{tmPage=Just c}) k

withBase :: URLString -> TM a -> TM a
withBase u t = withEnv (\ e -> e{tmBase=u}) t

withDefaultArgs :: [(String,String)] -> TM a -> TM a
withDefaultArgs as t = withEnv (\ e -> e{tmDefaultArgs=as}) t

-- | @fromSource "foo" action@ indicates that @foo@ is the source
-- of the twitter. Not all Twitter API actions currently like being 
-- passed a @source=@ argument (e.g., search actions), so you may have
-- to be selective about where you apply this action..
fromSource :: String -> TM a -> TM a
fromSource src t = withEnv (\ e -> e{tmSource=Just src}) t

withPageCount :: Maybe Int -> Maybe Int -> TM a -> TM a
withPageCount mbP mbC k = withEnv (\e -> e{tmPage=mbP,tmCount=mbC}) k

withAuth :: Bool -> TM a -> TM a
withAuth False tm = withEnv (\e -> e{tmUser=Nothing}) tm
withAuth _ tm = tm

postMethod :: TM a -> TM a
postMethod (TM x) = TM $ \ env -> x env{tmPost=True}

getPostFlag :: TM Bool
getPostFlag = getEnv >>= return.tmPost

getUser :: TM (Maybe AuthUser)
getUser = TM $ \ env -> return (tmUser env)

getEnv :: TM TMEnv
getEnv = TM $ \ env -> return env

getCount :: TM (Maybe Int)
getCount = TM $ \ env -> return (tmCount env)

getPage :: TM (Maybe Int)
getPage = TM $ \ env -> return (tmPage env)

getPageCount :: TM (Maybe Int, Maybe Int)
getPageCount = TM $ \ env -> return (tmCount env, tmPage env)

-- | @getDefArgs@ returns the /default/ arguments to pass to all
-- Twitter API actions. The /default-default/ is the empty list.
-- See also 'getSource' / 'fromSource'.
getDefArgs :: TM [(String,String)]
getDefArgs = TM $ \ env -> return (tmDefaultArgs env)

getSource :: TM (Maybe String)
getSource = TM $ \ env -> return (tmSource env)

getBase :: TM URLString
getBase = TM $ \ env -> return (tmBase env)

liftIO :: IO a -> TM a
liftIO a = TM $ \ _ -> a

runTwitter :: Maybe AuthUser -> URLString -> TM a -> IO a
runTwitter mbu b dm = (unTM dm) nullEnv{tmUser=mbu,tmBase=b}

runTM :: AuthUser -> TM a -> IO a
runTM user a = runTwitter (Just user) api_base a

