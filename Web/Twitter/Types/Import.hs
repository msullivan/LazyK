module Web.Twitter.Types.Import where

import Web.Twitter.Types

import Text.JSON
import Text.JSON.Types

import Data.Char
import Data.Maybe
import Control.Monad

import Debug.Trace

data JM a = JM (String -> [(String,JSValue)] -> Result a)

instance Monad JM where
  return x = JM (\ _ _ -> return x)
  (JM a) >>= k = JM $ \ loc env -> do
     v <- a loc env
     case k v of
      (JM b) -> b loc env
     
(-=>) :: a -> (b -> c) -> b -> (a,c)
(-=>) a b c = (a,b c)

runJM :: String -> [(String,JSValue)] -> JM a -> Result a
runJM loc ps (JM a) = a loc ps

catchJM :: JM a -> (String -> JM a) -> JM a
catchJM (JM a) h = JM $ \ loc env -> 
   case a loc env of
     Error s -> case h s of { ( JM x) -> x loc env }
     e@Ok{} -> e

liftR :: Result a -> JM a
liftR r = JM $ \ _ _ -> r

getLoc :: JM String
getLoc = JM (\ l _ -> return l)

getEnv :: JM [(String,JSValue)]
getEnv = JM (\ _ e -> return e)

addToEnv :: [(String,JSValue)] -> JM a -> JM a
addToEnv ls (JM x) = JM $ \ loc ps -> x loc (ls++ps)

get :: String -> JM String
get k = do -- trace (show ("g",k)) $ do
  m   <- getMb k
  case m of
    Just (JSString s)  -> return (fromJSString s)
    Just jso -> return (showJSValue jso "")
    Nothing -> do
       loc <- getLoc
       fail (loc ++ ": missing value for key " ++ show k)

getDefault :: String -> String -> JM String
getDefault def k = do
  m <- getMb k 
  case m of
    Just (JSString s)  -> return (fromJSString s)
    Just jso -> return (showJSValue jso "")
    Nothing -> do
       loc <- getLoc
       trace (loc ++ " - warning: expected value for key " ++ shows k " but found none.")
             (return def)

getInt :: String -> JM Integer
getInt k = do
  s <- get k
  case reads s of
    ((v,_):_) -> return v
    _ -> do
       loc <- getLoc
       fail (loc ++ ": expected valid int, got " ++ show s)

getMbS :: String -> JM (Maybe String)
getMbS k = do
  v <- getMb k
  case v of
    Just (JSString s) -> return (Just (fromJSString s))
    _ -> return Nothing

getMbI :: String -> JM (Maybe Int)
getMbI k = do
  m <- getMb k
  case m of
    Just v -> {-trace (show ("i",v)) $ -}liftR (readJSON v) >>= return . Just
    _ -> return Nothing

getMbJ :: JSON a => String -> JM (Maybe a)
getMbJ k = do
  v <- getMb k
  case v of
    Just j -> {-trace (show ("v",v)) $ -}liftR (readJSON j) >>= return . Just
    _      -> return Nothing

getJ :: JSON a => String -> JM a
getJ k = do
  v <- getMb k
  case v of
    Just j -> liftR (readJSON j)
    _      -> do
       loc <- getLoc
       fail (loc ++ ": unable to locate expected JSON field " ++ show k)

getArr :: String -> JM [JSValue]
getArr k = do
  v <- getMb k
  case v of
    Just (JSArray as) -> return as
    _      -> do
       loc <- getLoc
       fail (loc ++ ": unable to locate expected JSON (array) field " ++ show k)

getMbB :: String -> JM (Maybe Bool)
getMbB k = do
  v <- getMb k
  case v of
    Just (JSBool b) -> return (Just b)
    _ -> return Nothing

getMb :: String -> JM (Maybe JSValue)
getMb k = JM $ \ _loc env -> {-trace (show ("gm",k,env)) $  -}return (lookup k env)

getB :: String -> JM Bool
getB v = do
  b <- get v
  case map toLower b of
    "true"  -> return True
    "false" -> return False
    "0"     -> return False
    "1"     -> return True
    bs -> do
      loc <- getLoc
      fail (loc ++ ": expected valid bool, got " ++ show bs)
    
getJSON :: String -> JM JSValue
getJSON k = do
  m <- getMb k
  case m of
    Just x -> return x
    Nothing -> do
       loc <- getLoc
       fail (loc ++ ": missing value for key " ++ show k)


showJS :: (a -> [(String, JSValue)]) -> a -> JSValue
showJS f x = JSObject (toJSObject (f x))

readJS :: String -> String -> a -> (JM a) -> JSValue -> Result a
readJS _ _ n _ (JSArray []) = return n
readJS m q n f (JSArray [x]) = readJS m q n f x
readJS loc nm n f (JSObject (JSONObject [(x,ls@JSArray{})])) 
  | nm == x = readJS loc nm n f ls
readJS m _ d f (JSObject (JSONObject pairs)) = runJM m pairs (catchJM f showErr)
  where
    showErr s = do
      loc <- getLoc
      trace (loc ++ ": " ++ s)
            (return d)
      
readJS m _ _ _ v = fail (m ++ ": unexpected JSON value " ++ show v)

instance JSON User where
    showJSON u = showJS showUser u
    readJSON m = readJS "Web.Twitter.Types.User" "" nullUser readUser m
    
instance JSON Status where
    showJSON u = showJS showStatus u
    readJSON m = readJS "Web.Twitter.Types.Status" "" nullStatus readStatus m
    
instance JSON UserInfo where
    showJSON u = showJS showUserInfo u
    readJSON m = readJS "Web.Twitter.Types.UserInfo" "" nullUserInfo readUserInfo m
    
instance JSON DirectMessage where
    showJSON u = showJS showDM u
    readJSON m = readJS "Web.Twitter.Types.DirectMessage" "" nullDirectMessage readDM m
    
instance JSON RateLimit where
    showJSON u = showJS showRateLimit u
    readJSON m = readJS "Web.Twitter.Types.RateLimit" "" nullRateLimit readRateLimit m
    
instance JSON Trends where
    showJSON u = showJS showTrends u
    readJSON m = readJS "Web.Twitter.Types.Trends" "" nullTrends readTrends m

instance JSON SearchResult where
    showJSON u = showJS showSearchResult u
    readJSON m = readJS "Web.Twitter.Types.SearchResult" "results" nullSearchResult readSearchResult m

    readJSONs (JSObject (JSONObject (("results", JSArray xs):_))) = do
       mapM (\ x -> readJS "Web.Twitter.Types.SearchResult" "results" nullSearchResult readSearchResult x) xs

    readJSONs _  = Error ("Unable to read search results")

instance JSON UserID where
    showJSON u = showJSON (userID u)
    readJSON m = 
       case m of
	 JSRational _ v -> return (UserID (show ((round v) :: Integer)))
	 _ -> return (UserID (showJSValue m ""))

    readJSONs (JSObject (JSONObject (("ids", JSArray xs):_))) = do
       mapM (\ x -> readJS "Web.Twitter.Types.UserID" "id" nullUserID readUserID x) xs
    readJSONs (JSArray xs) = do
       mapM (\ x -> readJSON x) xs

    readJSONs x  = trace (show x) $ Error ("Unable to read user ID")
    

showUser :: User -> [(String, JSValue)]
showUser u = 
  [ "id"           -=> str  $ userId u
  , "name"         -=> str  $ userName u
  , "screen_name"  -=> str  $ userScreenName u
  , "description"  -=> str  $ userDescription u
  , "location"     -=> str  $ userLocation u
  ] ++ catMaybes
    [ mb "profile_image_url"  str  (userProfileImageURL u)
    , mb "url"                str  (userURL u)
    , mb "followers_count"    int  (userFollowers u)
    , mb "protected"          bool (userProtected u)
    ]

str :: String -> JSValue
str s  = showJSON (JSONString s)
int :: Int -> JSValue
int i  = showJSON (i::Int)
inte :: Integer -> JSValue
inte i  = showJSON (i::Integer)
bool :: Bool -> JSValue
bool f = showJSON (JSBool f)

arr :: [JSValue] -> JSValue
arr s  = showJSON (JSArray s)

obj :: [(String,JSValue)] -> JSValue
obj s  = showJSON (JSObject (JSONObject s))

js :: JSON a => a -> JSValue
js = showJSON

readB :: Maybe String -> JM (Maybe Bool)
readB Nothing  = return Nothing
readB (Just f) = return $ 
  case map toLower f of
    "true"  -> Just True
    "false" -> Just False
    "0"     -> Just False
    "1"     -> Just True
    _       -> Nothing

mb :: String -> (a -> b) -> Maybe a -> Maybe (String, b)
mb _ _ Nothing  = Nothing
mb t f (Just v) = Just (t,f v)

readUser :: JM User
readUser = do
  [i,nm,scr,des,loc] <- mapM (getDefault "")  ["id","name","screen_name","description","location"]
  prot <- getMbB "protected"
  mbU  <- getMbS "url"
  mbP  <- getMbS "profile_image_url"
  mbF  <- getMbI "followers_count"
  return nullUser
     { userId              = i
     , userName            = nm
     , userScreenName      = scr
     , userDescription     = des
     , userLocation        = loc
     , userProfileImageURL = mbP
     , userURL             = mbU
     , userProtected       = prot
     , userFollowers       = mbF
     }

showStatus :: Status -> [(String, JSValue)]
showStatus s = 
  [ "created_at"   -=> str  $ statusCreated s
  , "id"           -=> str  $ statusId s
  , "text"         -=> str  $ statusText s
  , "source"       -=> str  $ statusSource s
  , "truncated"    -=> bool $ statusTruncated s
  , "user"         -=> showJSON  $ statusUser s
  ] ++ catMaybes
    [ mb "in_reply_to_status_id"  str  (statusInReplyTo s)
    , mb "in_reply_to_user_id"    str  (statusInReplyToUser s)
    , mb "favorited"              bool (statusFavorite s)
    ]

readStatus :: JM Status
readStatus = do
  st <- getMb "status"
  augment <- 
    case st of
      Just (JSObject (JSONObject ps)) -> do
          ls <- getEnv
          return (addToEnv (("user", JSObject (JSONObject ls)):ps))
      _ -> return id
  augment $ do
    [cr,i,te,src] <- mapM get ["created_at","id","text","source"]
    u   <- getJ "user"
    tr  <- getB "truncated"
    inr <- getMbS "in_reply_to_status_id"
    inu <- getMbS "in_reply_to_user_id"
    fa  <- getMbB "favorited"
--    u   <- getMbJ "user" 
    return nullStatus
     { statusCreated       = cr
     , statusId            = i
     , statusText          = te
     , statusSource        = src
     , statusTruncated     = tr
     , statusInReplyTo     = inr
     , statusInReplyToUser = inu
     , statusFavorite      = fa
     , statusUser          = u
     }

showUserInfo :: UserInfo -> [(String, JSValue)]
showUserInfo u = 
  [ "profile_background_tile"       -=> bool  $ userInfoBackgroundTile u
  , "profile_link_color"            -=> str  $ userInfoLinkColor u
  , "profile_background_color"      -=> str  $ userInfoBackground u
  , "profile_text_color"            -=> str  $ userInfoTextColor u
  , "profile_sidebar_fill_color"    -=> str  $ userInfoSidebarFill u
  , "profile_sidebar_border_color"  -=> str  $ userInfoSidebarColor u
  , "profile_background_image_url"  -=> str  $ userInfoBackgroundImageURL u
  , "followers_count"               -=> int  $ userInfoFollowers u
  , "description"                   -=> str  $ userInfoDescription u
  , "utc_offset"                    -=> int  $ userInfoUTCOffset u
  , "favourites_count"              -=> int  $ userInfoFavorites u
  , "created_at"                    -=> str  $ userInfoCreated u
  , "time_zone"                     -=> str  $ userInfoTimezone u
  , "profile_image_url"             -=> str  $ userInfoImageURL u
  , "statuses_count"                -=> int  $ userInfoStatusCount u
  , "friends_count"                 -=> int  $ userInfoFriends u
  , "screen_name"                   -=> str  $ userInfoScreenName u
  , "protected"                     -=> bool $ userInfoProtected u
  , "location"                      -=> str  $ userInfoLocation u
  , "name"                          -=> str  $ userInfoName u
  , "id"                            -=> str  $ userInfoId u
  ] ++ catMaybes
    [ mb "url"     str  (userInfoURL u)
    ]

readUserInfo :: JM UserInfo
readUserInfo = do
  [ lc,bg,tc
   , sf,sbg,burl
   , desc,cre,tz
    , sn,loc
     , nm,i] 
     <- mapM (getDefault "")
                  [ "profile_link_color"
                  , "profile_background_color"
                  , "profile_text_color"
                  , "profile_sidebar_fill_color"
                  , "profile_sidebar_border_color"
                  , "profile_background_image_url"
                  , "description"
		  , "created_at"
		  , "time_zone"
		  , "screen_name"
		  , "location"
		  , "name"
		  , "id"
		  ]
  prot <- getB   "protected"
  bgt  <- getB   "profile_background_tile"
  fo   <- getInt "followers_count"
  utc  <- getInt "utc_offset"
  fav  <- getInt "favourites_count"
  sc   <- getInt "statuses_count"        
  fri  <- getInt "friends_count"
  imgurl <- getDefault "" "profile_image_url"
  iurl   <- getMbS "url"
  return UserInfo
     { userInfoBackgroundTile = bgt
     , userInfoLinkColor      = lc
     , userInfoBackground     = bg
     , userInfoBackgroundImageURL  = burl
     , userInfoTextColor      = tc
     , userInfoSidebarFill    = sf
     , userInfoSidebarColor   = sbg
     , userInfoFollowers      = fromInteger fo
     , userInfoDescription    = desc
     , userInfoUTCOffset      = fromInteger utc
     , userInfoFavorites      = fromInteger fav
     , userInfoCreated        = cre
     , userInfoTimezone       = tz
     , userInfoImageURL       = imgurl
     , userInfoURL            = iurl
     , userInfoStatusCount    = fromInteger sc
     , userInfoFriends        = fromInteger fri
     , userInfoScreenName     = sn
     , userInfoProtected      = prot
     , userInfoLocation       = loc
     , userInfoName           = nm
     , userInfoId             = i
     }

showDM :: DirectMessage -> [(String, JSValue)]
showDM s =
  [ "sender_id"        -=> str  $ directSenderId s
  , "recipient_id"     -=> str  $ directRecipientId s
  , "recipient_screen_name"     -=> str  $ directRecipientName s
  , "sender_screen_name"     -=> str  $ directSenderName s
  , "text"         -=> str $ directText s
  , "id"           -=> str $ directId s
  , "created_at"   -=> str $ directCreated s
  ] ++ catMaybes
    [ mb "recipient"    js (directRecipient s)
    , mb "sender"       js (directSender s)
    ]

readDM :: JM DirectMessage
readDM = do
  [sid,rid,rscr,sscr,txt,i,cre] <- 
     mapM (getDefault "")
          ["sender_id","recipient_id","recipient_screen_name", "sender_screen_name", "text", "id", "created_at"]
  rec <- getMbJ "recipient"
  sen <- getMbJ "sender"
  return nullDirectMessage
     { directSenderId      = sid
     , directRecipientId   = rid
     , directRecipientName = rscr
     , directSenderName    = sscr
     , directText          = txt
     , directId            = i
     , directCreated       = cre
     , directRecipient     = rec
     , directSender        = sen
     }

showRateLimit :: RateLimit -> [(String, JSValue)]
showRateLimit r = 
  [ "reset_time_in_seconds"  -=> inte  $ rateLimitResetSecs r
  , "reset_time"         -=> str  $ rateLimitResetTime r
  , "remaining_hits"     -=> inte  $ rateLimitRemHits r
  , "hourly_limit"       -=> inte  $ rateLimitHourlyLimit r
  ]

readRateLimit :: JM RateLimit
readRateLimit = do
  rs <- getInt "reset_time_in_seconds"
  rt <- getDefault ""  "reset_time"
  rh <- getInt "remaining_hits"
  rl <- getInt "hourly_limit"
  return nullRateLimit
      { rateLimitResetSecs = rs
      , rateLimitResetTime = rt
      , rateLimitRemHits   = rh
      , rateLimitHourlyLimit = rl
      }


showTrends :: Trends -> [(String, JSValue)]
showTrends t = 
  [ "as_of"   -=> str  $ trendsAsOf t
  , "trends"  -=> arr  $ (map (\ (a,b) -> obj [("name", showJSON a), ("url", showJSON b)])
                              (trendsInfo t))
  ]

readTrends :: JM Trends
readTrends = do
  rs <- get "as_of"
  as <- getArr "trends"
  ls <- mapM readEntry as
  return nullTrends
      { trendsAsOf = rs
      , trendsInfo = ls
      }
  where 
   readEntry (JSObject (JSONObject os)) = addToEnv os $ do
      a <- get "name"
      b <- get "url"
      return (a,b)
   readEntry _ = return ("","")
      

showSearchResult :: SearchResult -> [(String,JSValue)]
showSearchResult s = 
  [ "text"  -=> str  $ searchResultText s
  , "id"    -=> str  $ searchResultId s
  , "from_user" -=> str $ searchResultFromUser s
  , "from_user_id" -=> str $ searchResultFromUserId s
  , "created_at"    -=> str $ searchResultAt s
  ] ++ catMaybes
   [  mb "language_code" str (searchResultLanguage s)
   ,  mb "to_user_id"    str (searchResultToUserId s)
   ,  mb "to_user"       str (searchResultToUser s)
   ]

readSearchResult :: JM SearchResult
readSearchResult = do
  [tx,i,fu,fui,ca] <- mapM (getDefault "")  ["text", "id", "from_user", "from_user_id", "created_at"]
  l   <- getMbS "language_code"
  tu  <- getMbS "to_user"
  tui <- getMbS "to_user_id"
  return nullSearchResult
     { searchResultText = tx
     , searchResultId   = i
     , searchResultFromUser = fu
     , searchResultFromUserId = fui
     , searchResultAt = ca
     , searchResultLanguage = l
     , searchResultToUser   = tu
     , searchResultToUserId = tui
     }

readUserID :: JM UserID
readUserID = do
  e <- getEnv
  u <- trace (show e) $ get "id"
  return UserID{userID=u}
