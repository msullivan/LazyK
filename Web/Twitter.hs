--------------------------------------------------------------------
-- |
-- Module      : Web.Twitter
-- Description : Toplevel module for the Twitter API
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Toplevel module for the Twitter API, providing entry points
-- to the various REST endpoints that twitter.com offer up
-- 
--------------------------------------------------------------------
module Web.Twitter 
       ( getPublicTimeline   -- :: TM [Status]
       , getFriendsTimeline  -- :: Maybe DateString -> Maybe String -> TM [Status]
       , getUserTimeline     -- :: Maybe String -> Maybe DateString -> Maybe String -> TM [Status]
       , getMentions         -- :: Maybe String -> Maybe String -> TM [Status]

       , showStatus          -- :: String -> TM Status
       , update              -- :: String -> Maybe String -> TM ()
       , getReplies          -- :: Maybe DateString -> Maybe String -> TM [Status]
       , destroyStatus       -- :: String -> TM ()

       , getFriends          -- :: Maybe String -> TM [Status]
       , getFollowers        -- :: Maybe String -> TM [Status]
       , getUserInfo         -- :: Maybe String -> Maybe String -> TM UserInfo

       , getDirectMessages     -- :: Maybe DateString -> Maybe String -> TM [DirectMessage]
       , getDirectMessagesSent -- :: Maybe DateString -> Maybe String -> TM [DirectMessage]
       , sendDirectMessage     -- :: UserId -> String -> TM DirectMessage
       , destroyDirectMessage  -- :: UserId -> TM ()

       , createFriend          -- :: UserId -> Maybe Bool -> TM User
       , destroyFriend         -- :: UserId -> TM ()
       , isFriendOf            -- :: UserId -> UserId -> TM Bool
       
       , getUserFollowers      -- :: Maybe String -> Maybe UserId -> Maybe String -> TM [UserId]
       , getUserFollowing      -- :: Maybe String -> Maybe UserId -> Maybe String -> TM [UserId]

       , verifyCredentials     -- :: TM User
       , endSession            -- :: TM ()
       , updateDeliveryDevice  -- :: Maybe String -> TM ()

       , ProfileColors(..)
       , nullProfileColors
       , updateProfileColors   -- :: ProfileColors -> TM ()

       , updateProfileImage           -- :: FilePath -> TM ()
       , updateProfileBackgroundImage -- :: FilePath -> TM ()
       
       , RateLimit(..)
       , nullRateLimit
       , getRateLimit   -- :: TM RateLimit

       , ProfileInfo(..)
       , nullProfileInfo
       , updateProfile  -- :: ProfileInfo -> TM ()

       , getFavorites    -- :: Maybe UserId -> TM [Status]
       , createFavorite  -- :: UserId -> TM User
       , destroyFavorite -- :: UserId -> TM User

       , followUser      -- :: UserId -> TM User
       , leaveUser       -- :: UserId -> TM User

       , createBlock     -- :: UserId -> TM User
       , destroyBlock    -- :: UserId -> TM User

       , testCall        -- :: TM String
       
       , search          -- :: String -> Maybe SearchContext -> TM [Status]
       , getTrends       -- :: TM [Status]
       , SearchContext(..)
       , searchFor
       
       , setUpdateInterval
       , setTwitterUserBS
       , setTwitterUser
       , tweet
       , tweetTM
       , stopUpdates
       , addSearchFilter
       , dropSearch

       ) where

import Web.Twitter.Types hiding ( URLString )
import Web.Twitter.Types.Import hiding ( showStatus )
import Web.Twitter.Monad
import Web.Twitter.Fetch
import Web.Twitter.Post

import Data.Maybe

-- for the silly stuff below
import Control.Concurrent
import Control.Monad
import System.IO.Unsafe
import System.Time
import System.Locale

--------------------------------------------------
-- A bit of persistent fun to enable use from within GHCi
--
twitter_user :: MVar (Maybe AuthUser)
twitter_user = unsafePerformIO (newMVar Nothing)

twitter_update_info :: MVar (Maybe Int,Maybe ThreadId)
twitter_update_info = unsafePerformIO (newMVar (Nothing,Nothing))

twitter_get_action :: MVar (Maybe String -> TM [Status])
twitter_get_action = unsafePerformIO (newMVar (\ _ -> return []))

twitter_searches :: MVar (SearchId, [(SearchId,SearchContext)],Maybe ThreadId)
twitter_searches = unsafePerformIO (newMVar (0,[],Nothing))

setUpdateInterval :: IO ()
setUpdateInterval = do
  putStr "Check updates every X mins: "
  l <- getLine
  case reads l of
    ((v,_):_) -> do
       (_,b) <- readMVar twitter_update_info
         -- kill old worker thread and start up new.
       case b of
         Nothing -> return ()
	 Just t  -> catch (killThread t) (\ _ -> return ())
       modifyMVar_ twitter_get_action 
                   (\ _ -> return (\ x -> getFriendsTimeline x Nothing))
       t <- forkIO (updateChecker v Nothing)
       modifyMVar_ twitter_update_info (\ _ -> return (Just v, Just t)) 
    _ -> putStrLn ("Unable to parse minute: "  ++ show l)
 where
  updateChecker everyMins mbSince  = do
    threadDelay (everyMins * 1000000 * 60)
    x <- readMVar twitter_user
    case x of
      Nothing -> updateChecker everyMins mbSince
      Just au -> do
	 n   <- nowDateString
	 upd <- readMVar twitter_get_action
         ls  <- runTM au (upd mbSince)
	 when (not $ null ls) (putStrLn "")
	 mapM_ (\ s -> putStrLn (userScreenName (statusUser s) ++ ": " ++ statusText s)) ls
	 updateChecker everyMins (Just n)

type SearchId = Int

addSearchFilter :: SearchContext -> IO SearchId
addSearchFilter sc = do
  st <- readMVar twitter_searches
  t  <- 
    case st of
     (_,_,Just t) -> return t
     (_,_,Nothing) -> do
          putStr "Perform tracking/searches every X mins: "
          l <- getLine
	  let readIt x = case reads x of { ((v,_):_) -> v ; _ -> 1} 
	  forkIO (searchBot (readIt l) Nothing)
  (a,b,_) <- takeMVar twitter_searches
  putMVar twitter_searches (a+1,(a,sc):b,Just t)
  return (a+1)
 where
  searchBot p mbSinceId = do
    threadDelay (p * 1000000 * 60)
    x <- readMVar twitter_searches
    u <- readMVar twitter_user
    case (u,x) of
      (Nothing,_) -> searchBot p mbSinceId
      (_, (_,[],_)) -> searchBot p mbSinceId
      (Just au, (_,ls,_)) -> do
	 n   <- nowDateString
	 let doOneSearch st@(sid,s) = do
	        ss <- runTM au (search s)
		case ss of
		  [] -> return st
		  _  -> do
		    let ifn "" c = c
		        ifn c  _ = c
		    let label = searchQuery s   `ifn` 
		                searchPhrase s  `ifn` 
				searchHashTag s `ifn` 
				  ("<"++shows sid ">")

		    putStrLn ("Search results for: " ++ label)
		    mapM_ ( \ r -> putStrLn (searchResultFromUser r ++ ": " ++ 
		                             searchResultText r ++ " @ " ++ 
					     searchResultAt r))
			  ss
                       -- update the sinceId so as to not repeat entries next time around..
		    return (sid,s{searchSinceId=searchResultId (head ss)})
         ls1 <- mapM doOneSearch ls
	 let updateSC y@(s,_) =
		   -- replace with new sinceId
	       maybe y (\ v -> (s,v)) (lookup s ls1)
	 modifyMVar_ twitter_searches (\ (a,xs,b) -> return (a,map updateSC xs,b))
	 searchBot p (Just n)

dropSearch :: SearchId -> IO ()
dropSearch s = do
  (a,ls,b) <- takeMVar twitter_searches
  ls1 <- 
    case break(\ x -> fst x == s) ls of
      (_,[]) -> do
         putStrLn ("Unknown search ID; ignoring")
         return ls
      (as,_:bs) -> 
         return (as++bs)
  putMVar twitter_searches (a,ls1,b)

stopUpdates :: IO ()
stopUpdates = do
       (a,b) <- readMVar twitter_update_info
         -- kill old worker thread and start up new.
       case b of
         Nothing -> return ()
	 Just t  -> catch (killThread t) (\ _ -> return ())
       modifyMVar_ twitter_update_info (\ _ -> return (a,Nothing))

setTwitterUserBS :: IO ()
setTwitterUserBS = do
   putStr "User name: "
   u <- getLine
   putStr "User password: "
   p <- getLine
   modifyMVar_ twitter_user (\ _ -> return $ Just (AuthUser u p)) 

setTwitterUser :: String -> String -> IO ()
setTwitterUser u p = do
   modifyMVar_ twitter_user (\ _ -> return $ Just (AuthUser u p)) 


tweet :: String -> IO ()
tweet s = do
   r <- readMVar twitter_user
   case r of
     Nothing -> do
       putStrLn "Unable to tweet, no user set - run 'setTwitterUser'" -- "
       return ()
     Just au -> do
       runTM au (update s Nothing)
       return ()

tweetTM :: TM a -> IO a
tweetTM act = do
   r <- readMVar twitter_user
   case r of
     Nothing -> do
       fail "Unable to run tweeting action; no user set - run 'setTwitterUser'" -- "
     Just au -> do
       runTM au act

nowDateString :: IO String
nowDateString = do
  c <- getClockTime
  return (formatDateString $ toUTCTime c)

formatDateString :: CalendarTime -> String
formatDateString ct = formatCalendarTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" ct

--------------------------------------------------

-- | @getPublicTimeline@ returns the 20 most recent statuses from non-protected 
-- users who have set a custom user icon
getPublicTimeline :: TM [Status]
getPublicTimeline = withAuth False $ restCall pub [] >>= readResult "getPublicTimeline"
  where pub = "public_timeline.json"

-- | @getFriendsTimeline mbSince mbSinceId@ returns the 20 most recent statuses posted by 
-- the authenticating user and that user's friends. Optionally constrained by start date
-- or a status ID.
getFriendsTimeline :: Maybe DateString -> Maybe String -> TM [Status]
getFriendsTimeline since sinceId = withAuth True $ restCall fri 
       (mbArg "since" since $
	  mbArg "since_id" sinceId []) >>= readResult "getFriendsTimeline"
  where
   fri = "friends_timeline.json"
   
-- | @getUserTimeline mbId mbSince mbSinceId@ returns the 20 most recent statuses 
-- posted from the authenticating user. It's also possible to request another user's 
-- timeline via the id parameter below. 
getUserTimeline :: Maybe String -> Maybe DateString -> Maybe String -> TM [Status]
getUserTimeline mbId since sinceId = do
  withAuth True $ 
   restCall usr
       (mbArg "id" mbId $
        mbArg "since" since $
	mbArg "since_id" sinceId []) >>= readResult "getUserTimeline"
  where
   usr = "user_timeline.json"
   
-- | @getMentions@ mbId mbMax@ returns the 20 most recent 
-- mentions (status containing @username) for the 
-- authenticating user.
getMentions :: Maybe String -> Maybe String -> TM [Status]
getMentions mbId mbMax = withAuth True $ 
   restCall usr
       (mbArg "max_id" mbMax $
	mbArg "since_id" mbId []) >>= readResult "getMentions"
  where
   usr = "mentions.json"


-- | @showStatus id@ returns a single status, specified by the @id@ parameter.
-- The status's author will be returned inline.
showStatus :: String -> TM Status
showStatus i = withAuth True $ restCall usr [] >>= readResult "showStatus"
  where
   usr = "show/" ++ i ++ ".json"
   
-- | @update text mbReplyToId@ updates the authenticating user's status to @text@.
update :: String -> Maybe String -> TM ()
update txt mbRep = withAuth True $ postMethod $ do
   restCall upd
            (arg "status" txt $
	     mbArg "in_reply_to_status_id" mbRep [])
   return ()
  where
   upd = "update.json"

-- | @getReplies mbSince mbSinceId@ returns the 20 most recent
-- \@replies (status updates prefixed with \@username) for the
-- authenticating user.
getReplies :: Maybe DateString -> Maybe String -> TM [Status]
getReplies since sinceId = withAuth True $ 
   restCall rep
       (mbArg "since" since $
	mbArg "since_id" sinceId []) >>= readResult "getReplies"
  where
   rep = "replies.json"
   
-- | @destroyStatus id@ destroys the status specified by the @id@
-- parameter.  The authenticating user must be the author of the 
-- specified status.
destroyStatus :: String -> TM ()
destroyStatus  i = withAuth True $ postMethod $ restCall des [] >> return ()
  where
   des = "destroy/" ++ i ++ ".json"

-- | @getFriends mbId@ returns up to 100 of the authenticating
-- user's friends who have most recently updated, each with current
-- status inline. It's also possible to request another user's 
-- recent friends list via the @mbId@ parameter.
getFriends :: Maybe String -> TM [Status]
getFriends mbId = withAuth True $ restCall fri [] >>= readResult "getFriends"
  where
   fri = 
     case mbId of
       Nothing -> "friends.json"
       Just i  -> "friends/" ++ i ++ ".json"
  
-- | @getFollowers mbId@ returns the authenticating user's followers,
-- each with current status inline.  They are ordered by the order in which 
-- they joined Twitter (this is going to be changed). 
getFollowers :: Maybe String -> TM [Status]
getFollowers mbId = withAuth True $ restCall folly [] >>= readResult "getFollowers"
  where
   folly = maybe "followers.json" (\ i -> "followers/" ++ i ++ ".json") mbId
  
-- | @getUserInfo mbId mbEmail@ returns extended information of a given user,
-- specified by ID or screen name as per the @mbId@ parameter below. 
-- This information includes design settings, so third party developers
-- can theme their widgets according to a given user's preferences.
-- You must be properly authenticated to request the page of a protected user.
getUserInfo :: Maybe String -> Maybe String -> TM UserInfo
getUserInfo mbId mbEmail = withBase user_base_url $ withAuth True $ 
    restCall folly 
              (mbArg "email" mbEmail []) >>= readResult "getUserInfo"
  where
   folly = maybe "users/show.json" (\i -> "users/show/" ++ i ++ ".json") mbId
  
-- | @getDirectMesssages mbSince mbSinceId@ returns a list of the 20 most
-- recent direct messages sent to the authenticating user.
getDirectMessages :: Maybe DateString -> Maybe String -> TM [DirectMessage]
getDirectMessages since sinceId = withBase top_base_url $ withAuth True $
   restCall rep
       (mbArg "since" since $
	mbArg "since_id" sinceId []) >>= readResult "getDirectMessages"
  where
   rep = "direct_messages.json"

-- | @getDirectMessagesSent mbSince mbSinceId@ returns a list of the 20 most
-- recent direct messages sent by the authenticating user.
getDirectMessagesSent :: Maybe DateString -> Maybe String -> TM [DirectMessage]
getDirectMessagesSent since sinceId = withBase top_base_url $ withAuth True $
   restCall rep
       (mbArg "since" since $
	mbArg "since_id" sinceId []) >>= readResult "getDirectMessagesSent"
  where
   rep = "direct_messages/sent.json"

-- | @sendDirectMessage userId text@ sends a new direct message to
-- the specified user from the authenticating user.  
-- Requires both the @user@ and @text@ parameters.
-- Returns the sent message in the requested format when successful.  
sendDirectMessage :: UserId -> String -> TM DirectMessage
sendDirectMessage uId txt = withBase top_base_url $ withAuth True $ postMethod $ 
   restCall dir
            (arg "user" uId $ arg "text" txt []) >>= readResult "sendDirectMessage"
  where
   dir = "direct_messages/new.json"

-- @destroyDirectMessage id@ destroys the direct message specified
-- in the required ID parameter.  The authenticating user must be the
-- recipient of the specified direct message.
destroyDirectMessage :: UserId -> TM ()
destroyDirectMessage i = withBase top_base_url $ withAuth True $ postMethod $ restCall des [] >> return ()
  where
   des = "direct_messages/destroy/" ++ i ++ ".json"

-- | @createFriend id mbFollow@ befriends the user specified in the @id@
-- parameter as the authenticating user.  Returns the befriended user in
-- the requested format when successful.  Returns a string describing the
-- failure condition when unsuccessful.
createFriend :: UserId -> Maybe Bool -> TM User
createFriend i mbFollow = withBase top_base_url $ withAuth True $ postMethod $ 
   restCall dir
            (mbArg "user" (fmap toB mbFollow) []) >>= readResult "createFriend"
  where
   dir = "friendships/create/" ++ i ++ ".json"

-- | @destroyFriend i@ discontinues friendship with the user specified in
-- the @id@ parameter as the authenticating user.  Returns the un-friended user
-- in the requested format when successful.  Returns a string describing the 
-- failure condition when unsuccessful.  
destroyFriend :: UserId -> TM User
destroyFriend i = withBase top_base_url $ withAuth True $ postMethod $ 
    restCall des [] >>= readResult "destroyFriend"
  where
   des = "friendships/destroy/" ++ i ++ ".json"

-- | @isFriendOf userA userB@ tests if a friendship exists between two users.
isFriendOf :: UserId -> UserId -> TM Bool
isFriendOf ua ub = withBase top_base_url $ withAuth True $ 
  restCall fr (arg "user_a" ua $ arg "user_b" ub []) >>= readResult "isFriendOf"
 where
  fr = "friendships/exists.json"

toB :: Bool -> String
toB False = "false"
toB True  = "true"

-- | @getUserFollowing mbId mbUser mbScreen@ returns a list of numeric IDs for every user
-- the given user is following.
getUserFollowing :: Maybe String -> Maybe String -> Maybe String -> TM [UserId]
getUserFollowing mbId mbUserId mbScreen = withBase top_base_url $ withAuth True $ 
  restCall fr (mbArg  "id" mbId $ 
                mbArg "user_id" mbUserId $
		 mbArg "screen_name" mbScreen []) >>= readResult "getUserFollowing" >>= return.(map userID)
 where
  fr = "friends/ids.json"

-- | @getUserFollowers mbId mbUser mbScreen@ returns a list of numeric IDs for every user
-- following the given user.
getUserFollowers :: Maybe String -> Maybe String -> Maybe String -> TM [UserId]
getUserFollowers mbId mbUserId mbScreen = withBase top_base_url $ withAuth True $ 
  restCall fr (mbArg  "id" mbId $ 
                mbArg "user_id" mbUserId $
		 mbArg "screen_name" mbScreen []) >>= readResult "getUserFollowers" >>= return.(map userID)
 where
  fr = "followers/ids.json"

-- | @verifyCredentials@ returns an HTTP 200 OK response code and a 
-- representation of the requesting user if authentication was successful; 
-- returns a 401 status code and an error message if not.
-- Use this method to test if supplied user credentials are valid.
verifyCredentials :: TM User
verifyCredentials = withBase acc_base_url $ withAuth True $
  restCall acc [] >>= readResult "verifyCredentials"
 where
  acc = "verify_credentials.json"

-- | @endSession@ ends the session of the authenticating user, 
-- returning a null cookie.  Use this method to sign users out of 
-- client-facing applications like widgets.
endSession :: TM ()
endSession = withBase acc_base_url $ withAuth True $ postMethod $
  restCall acc [] >> return ()
 where
  acc = "end_session"

-- | @updateDeliveryService mbServ@ sets which device Twitter delivers
-- updates to for the authenticating user.  Sending @Nothing@ as the
-- device parameter will disable IM(@im@) or SMS(@sms@) updates.
updateDeliveryDevice :: Maybe String -> TM ()
updateDeliveryDevice mbS = withBase acc_base_url $ withAuth True $ postMethod $
  restCall acc (arg "device" (fromMaybe "none" mbS) []) >> return ()
 where
  acc = "update_delivery_device.json"

data ProfileColors
 = ProfileColors
     { profileTextColor   :: Maybe ColorString
     , profileBackColor   :: Maybe ColorString
     , profileLinkColor   :: Maybe ColorString
     , profileSidebarFill :: Maybe ColorString
     , profileSidebarBorder :: Maybe ColorString
     }

nullProfileColors :: ProfileColors
nullProfileColors
 = ProfileColors
     { profileTextColor   = Nothing
     , profileBackColor   = Nothing
     , profileLinkColor   = Nothing
     , profileSidebarFill = Nothing
     , profileSidebarBorder = Nothing
     }

-- | @updateProfileColors pc@ sets one or more hex values that control the
-- color scheme of the authenticating user's profile page on @twitter.com@.
updateProfileColors :: ProfileColors -> TM ()
updateProfileColors pc = withBase acc_base_url $ withAuth True $ postMethod $
  restCall acc 
           (mbArg "profile_background_color" (profileBackColor pc) $
	    mbArg "profile_text_color"       (profileTextColor pc) $
	    mbArg "profile_link_color"       (profileLinkColor pc) $
	    mbArg "profile_sidebar_fill_color" (profileSidebarFill pc) $
	    mbArg "profile_sidebar_border_color" (profileSidebarFill pc) $
	    []) >> return ()
 where
  acc = "update_profile_colors.json"

-- | @updateProfileImage imgFile@ updates the authenticating user's profile image.
-- Expects raw multipart data, not a URL to an image.
updateProfileImage :: FilePath -> TM ()
updateProfileImage fp = withBase acc_base_url $ withAuth True $ postMethod $ do
     let pr = 
          addNameFile "image" fp Nothing $
	     newPostRequest "img_upload"
     (url_q, hs, bod) <- liftIO (toRequest pr (Just PostFormData))
     let u = appendQueryArgs acc url_q
     (_,_,_) <- postCall u hs bod []
     return ()
  where
   acc = "update_profile_image.json"

-- | @updateProfileBackgroundImage imgFile@ udates the authenticating
-- user's profile background image.  Expects raw multipart data, not a 
-- URL to an image.
updateProfileBackgroundImage :: FilePath -> TM ()
updateProfileBackgroundImage fp = withBase acc_base_url $ withAuth True $ postMethod $ do
     let pr = 
          addNameFile "image" fp Nothing $
	     newPostRequest "img_upload"
     (url_q, hs, bod) <- liftIO (toRequest pr (Just PostFormData))
     let u = appendQueryArgs acc url_q
     (_,_,_) <- postCall u hs bod []
     return ()
  where
   acc = "update_profile_background_image.json"

-- | @getRateLimit@ returns the remaining number of API requests available
-- to the requesting user before the API limit is reached for the current
-- hour. Calls to @getRateLimit@ do not count against the rate limit.
-- If authentication credentials are provided, the rate limit status for
-- the authenticating user is returned.  Otherwise, the rate limit status
-- for the requester's IP address is returned.
getRateLimit :: TM RateLimit
getRateLimit = withBase acc_base_url $ withAuth True $ do
  restCall acc [] >>= readResult "getRateLimit"
  where
   acc = "rate_limit_status.json"

appendQueryArgs :: String -> String -> String
appendQueryArgs x "" = x
appendQueryArgs x y  = x ++ '?':y

data ProfileInfo
 = ProfileInfo
     { profileInfoName        :: Maybe String
     , profileInfoEmail       :: Maybe String
     , profileInfoURL         :: Maybe URLString
     , profileInfoLocation    :: Maybe String
     , profileInfoDescription :: Maybe String
     }

nullProfileInfo :: ProfileInfo
nullProfileInfo = ProfileInfo
     { profileInfoName        = Nothing
     , profileInfoEmail       = Nothing
     , profileInfoURL         = Nothing
     , profileInfoLocation    = Nothing
     , profileInfoDescription = Nothing
     }

-- | @updateProfile profileInfo@ sets values that users are able to
-- set under the "Account" tab of their settings page. Only the parameters
-- specified will be updated; to only update the "name" attribute, for
-- example, only include that as a @Just@ value in the @ProfileInfo@ parameter.
updateProfile :: ProfileInfo -> TM ()
updateProfile pin = withBase acc_base_url $ withAuth True $ postMethod $
  restCall acc 
           (mbArg "name"        (profileInfoName pin) $
	    mbArg "email"       (profileInfoEmail pin) $
	    mbArg "url"         (profileInfoURL pin) $
	    mbArg "location"    (profileInfoLocation pin) $
	    mbArg "description" (profileInfoDescription pin) $
	    []) >> return ()
 where
  acc = "update_profile.json"


-- | @getFavorites mbId@ returns the 20 most recent favorite statuses
-- for the authenticating user or user specified by the @mbId@ parameter.
getFavorites :: Maybe UserId -> TM [Status]
getFavorites i = withBase top_base_url $ withAuth True $ 
  restCall acc [] >>= readResult "getFavorites"
 where
  acc = maybe "favorites.json" (\ x -> "favorites/"++x++".json") i

-- | @createFavorite id@ favorites the status specified in the @id@
-- parameter as the authenticating user.  
-- Returns the favorite status when successful.
createFavorite :: UserId -> TM User
createFavorite i = withBase top_base_url $ withAuth True $ postMethod $ 
   restCall dir [] >>= readResult "createFavorite"
  where
   dir = "favorites/create/"++i++".json"

-- | @destroyFavorite id@ un-favorites the status specified in
-- the ID parameter as the authenticating user.  
-- Returns the un-favorited status in the requested format when successful.  
destroyFavorite :: UserId -> TM User
destroyFavorite i = withBase top_base_url $ withAuth True $ postMethod $ restCall des [] >>= readResult "destroyFavorite"
  where
   des = "favorites/destroy/" ++ i ++ ".json"

-- | @followUser id@ enables notifications for updates from the
-- specified user to the authenticating user.  Returns the specified 
-- user when successful.
followUser :: UserId -> TM User
followUser i = withBase top_base_url $ withAuth True $ postMethod $ 
   restCall dir [] >>= readResult "followUser"
  where
   dir = "notifications/follow/"++i++".json"

-- | @leaveUser id@ disables notifications for updates from the
-- specified user to the authenticating user.
-- Returns the specified user when successful.
leaveUser :: UserId -> TM User
leaveUser i = withBase top_base_url $ withAuth True $ postMethod $ 
   restCall dir [] >>= readResult "leaveUser"
  where
   dir = "notifications/leave/"++i++".json"

-- | @createBlock id@ blocks the user specified in the @id@ parameter
-- as the authenticating user.  Returns the blocked user.
createBlock :: UserId -> TM User
createBlock i = withBase top_base_url $ withAuth True $ postMethod $ 
   restCall dir [] >>= readResult "createBlock"
  where
   dir = "blocks/create/"++i++".json"

-- | @destroyBlock id@ un-blocks the user specified in the @id@ parameter
-- as the authenticating user.  Returns the un-blocked user.
destroyBlock :: UserId -> TM User
destroyBlock i = withBase top_base_url $ withAuth True $ postMethod $ restCall des [] >>= readResult "destroyBlock"
  where
   des = "blocks/destroy/" ++ i ++ ".json"

-- | @testCall@ returns the string "ok" in the requested format 
-- with a 200 OK HTTP status code.
testCall :: TM String
testCall = withBase top_base_url $ withAuth False $ 
   restCall "help/test.json" [] >>= readResult "testCall"

-- The "Search API"
--

-- | @testCall@ returns the string "ok" in the requested format 
-- with a 200 OK HTTP status code.
getTrends :: TM Trends
getTrends = withBase search_base_url $ withAuth False $ do
   restCall "trends.json" [] >>= readResult "getTrends"

data SearchContext
 = SearchContext
       { searchLang      :: String
       , searchRPP       :: Int
       , searchPage      :: Int
       , searchSinceId   :: StatusId
       , searchGeocode   :: String
       , searchShowUser  :: Bool
       , searchQuery     :: String
       , searchHashTag   :: String

       , searchFromUser  :: UserName
       , searchToUser    :: UserName
       , searchReferring :: UserName
       , searchAllWords  :: [String]
       , searchAnyWords  :: [String]
       , searchNoneWords :: [String]
       , searchPhrase    :: String
       , searchNear      :: String -- location name
       }

searchFor :: SearchContext
searchFor = SearchContext
       { searchLang     = "all"
       , searchRPP      = 15
       , searchPage     = 1
       , searchSinceId  = ""
       , searchGeocode  = ""
       , searchShowUser = True
       , searchQuery    = ""
       , searchHashTag  = ""

       , searchFromUser  = ""
       , searchToUser    = ""
       , searchReferring = ""
       , searchAllWords  = []
       , searchAnyWords  = []
       , searchNoneWords = []
       , searchPhrase    = ""
       , searchNear      = ""
       }

search :: SearchContext
       -> TM [SearchResult]
search scon = withBase search_base_url $ 
  restCall acc (searchArgs scon []) >>= readResult "search"
 where
  acc = "search.json"

  searchArgs sc ls = 
     strArg "lang" (searchLang sc) $
     strArg "rpp"  (show $ searchRPP sc) $
     strArg "page" (show $ searchPage sc) $
     strArg "since_id" (searchSinceId sc) $
     strArg "geocode"  (searchGeocode sc) $ 
     strArg "show_user" (searchSinceId sc) $
     strArg "tag"       (searchHashTag sc) $
     strArg "q"    (searchQuery sc) $
     strArg "from" (searchFromUser sc) $
     strArg "to"   (searchToUser sc) $
     strArg "ref"  (searchReferring sc) $
     strArg "ands" (unwords $ searchAllWords sc) $
     strArg "ors"  (unwords $ searchAnyWords sc) $
     strArg "nots" (unwords $ searchNoneWords sc) $
     strArg "phrase" (searchPhrase sc) $
        -- this doesn't appear to work as expected via the .json interface
     strArg "near"   (searchNear sc) $
     strArg "within" ((\ x -> if x == "" then "" else "15") $ searchNear sc) $
     strArg "units"  ((\ x -> if x == "" then "" else "mi") $ searchNear sc) $
      ls
	    
