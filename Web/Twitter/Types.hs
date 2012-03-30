--------------------------------------------------------------------
-- |
-- Module      : Web.Twitter.Types
-- Description : Main types introduced by the Twitter API
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- This module defines the main types that the Haskell API binding
-- for Twitter introduces. Most importantly, if that's the word,
-- the representation of tweets\/status updates.
-- 
--------------------------------------------------------------------
module Web.Twitter.Types where

type ColorString = String
-- Thu Dec 25 21:07:16 +0000 2008 (sure there was a good reason not to use 8601..)
type DateString  = String
type UserId      = String
type URLString   = String
type UserName    = String
type StatusId    = String

data Format = FormatXML | FormatJSON | FormatRSS | FormatAtom

newtype UserID = UserID { userID :: UserId }

-- | @Status@ is the record type used to represent a ''tweet'',
-- a status update by a user.
data Status
 = Status
     { statusCreated       :: DateString
     , statusId            :: StatusId
     , statusText          :: String
     , statusSource        :: String
     , statusTruncated     :: Bool
     , statusInReplyTo     :: Maybe StatusId
     , statusInReplyToUser :: Maybe UserId
     , statusFavorite      :: Maybe Bool
     , statusUser          :: User
     }

nullStatus :: Status
nullStatus = Status
     { statusCreated       = ""
     , statusId            = ""
     , statusText          = ""
     , statusSource        = ""
     , statusTruncated     = False
     , statusInReplyTo     = Nothing
     , statusInReplyToUser = Nothing
     , statusFavorite      = Nothing
     , statusUser          = nullUser
     }

data User
  = User
     { userId              :: UserId
     , userName            :: UserName
     , userScreenName      :: String
     , userDescription     :: String
     , userLocation        :: String
     , userProfileImageURL :: Maybe URLString
     , userURL             :: Maybe URLString
     , userProtected       :: Maybe Bool
     , userFollowers       :: Maybe Int
     }

nullUser :: User
nullUser = User
     { userId              = ""
     , userName            = ""
     , userScreenName      = ""
     , userDescription     = ""
     , userLocation        = ""
     , userProfileImageURL = Nothing
     , userURL             = Nothing
     , userProtected       = Nothing
     , userFollowers       = Nothing
     }

data UserInfo
 = UserInfo
     { userInfoBackgroundTile :: Bool
     , userInfoLinkColor      :: ColorString
     , userInfoBackground     :: ColorString
     , userInfoBackgroundImageURL :: URLString
     , userInfoTextColor      :: ColorString
     , userInfoSidebarFill    :: ColorString
     , userInfoSidebarColor   :: ColorString
     , userInfoFollowers      :: Int
     , userInfoDescription    :: String
     , userInfoUTCOffset      :: Int
     , userInfoFavorites      :: Int
     , userInfoCreated        :: DateString
     , userInfoTimezone       :: String
     , userInfoImageURL       :: URLString
     , userInfoURL            :: Maybe URLString
     , userInfoStatusCount    :: Int
     , userInfoFriends        :: Int
     , userInfoScreenName     :: UserName
     , userInfoProtected      :: Bool
     , userInfoLocation       :: String
     , userInfoName           :: UserName
     , userInfoId             :: UserId
     }

nullUserInfo :: UserInfo
nullUserInfo = UserInfo
     { userInfoBackgroundTile = False
     , userInfoLinkColor      = ""
     , userInfoBackground     = ""
     , userInfoBackgroundImageURL  = ""
     , userInfoTextColor      = ""
     , userInfoSidebarFill    = ""
     , userInfoSidebarColor   = ""
     , userInfoFollowers      = 0
     , userInfoDescription    = ""
     , userInfoUTCOffset      = 0
     , userInfoFavorites      = 0
     , userInfoCreated        = ""
     , userInfoTimezone       = ""
     , userInfoImageURL       = ""
     , userInfoURL            = Nothing
     , userInfoStatusCount    = 0
     , userInfoFriends        = 0
     , userInfoScreenName     = ""
     , userInfoProtected      = False
     , userInfoLocation       = ""
     , userInfoName           = ""
     , userInfoId             = ""
     }

data DirectMessage
 = DirectMessage
     { directRecipient     :: Maybe User
     , directSender        :: Maybe User
     , directSenderId      :: UserId
     , directSenderName    :: UserName
     , directRecipientId   :: UserId
     , directRecipientName :: UserId
     , directText          :: String
     , directId            :: StatusId -- mis-named, but works.
     , directCreated       :: DateString
     }

nullDirectMessage :: DirectMessage
nullDirectMessage = DirectMessage
     { directRecipient     = Nothing
     , directSender        = Nothing
     , directSenderId      = ""
     , directSenderName    = ""
     , directRecipientId   = ""
     , directRecipientName = ""
     , directText          = ""
     , directId            = ""
     , directCreated       = ""
     }

-- | @RateLimit@ bundles up the data that the 'getRateLimit'
-- Twitter API call returns regarding API call limits.
data RateLimit
 = RateLimit
     { rateLimitResetSecs :: Integer
     , rateLimitResetTime :: DateString
     , rateLimitRemHits   :: Integer
     , rateLimitHourlyLimit :: Integer
     }

nullRateLimit :: RateLimit
nullRateLimit = RateLimit
     { rateLimitResetSecs = 0
     , rateLimitResetTime = ""
     , rateLimitRemHits   = 0
     , rateLimitHourlyLimit = 0
     }

-- | @Trends@ bundles up the data that the 'trends'
-- Twitter Search API call returns regarding what's
-- being majorly tweeted about.
data Trends
 = Trends
     { trendsAsOf :: DateString
     , trendsInfo :: [(String{-name-}, URLString)]
     }

nullTrends :: Trends
nullTrends = Trends
     { trendsAsOf = ""
     , trendsInfo = []
     }

-- | @SearchResult@ bundles up the data that the Search API returns.
data SearchResult
 = SearchResult
     { searchResultText       :: String
     , searchResultId         :: StatusId
     , searchResultFromUser   :: UserName
     , searchResultFromUserId :: UserId
     , searchResultAt         :: DateString
     , searchResultLanguage   :: Maybe String
     , searchResultToUser     :: Maybe UserName
     , searchResultToUserId   :: Maybe UserId
     }

nullSearchResult :: SearchResult
nullSearchResult = SearchResult
     { searchResultText       = ""
     , searchResultId         = ""
     , searchResultFromUser   = ""
     , searchResultFromUserId = ""
     , searchResultAt         = ""
     , searchResultLanguage   = Nothing
     , searchResultToUser     = Nothing
     , searchResultToUserId   = Nothing
     }

nullUserID :: UserID
nullUserID  = UserID { userID = "" }
