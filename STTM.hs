{-#LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Main
(main
) where 

import qualified LazyK as K
import Data.List
import System.Exit
import System(getArgs)
import System.IO
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe

import Web.Codec.URLEncoder
import Data.Hash.MD5
import qualified Data.Binary as Binary
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Text.JSON as JS
import Data.Time.Clock

import System.Process
import Control.Concurrent

type Id = String
data Atom = S | K | I | Inc | Num Int | Thunk Id
          deriving (Ord, Eq)

type Spine = [Atom]
type Thunk = (Spine, Bool)

instance Show Atom where
  show S = "S"
  show K = "K"
  show I = "I"
  show Inc = "+"
  show (Num i) = show i
  show (Thunk i) = "#t" ++ i

readAtom :: String -> Atom
readAtom "S" = S
readAtom "K" = K
readAtom "I" = I
readAtom "+" = Inc
readAtom s = if "#t" `isPrefixOf` s then Thunk (drop 2 s)
             else Num (read s)

findSubstr s t =
  if s `isPrefixOf` t then 0 else 1 + findSubstr s (tail t)

stripTag :: String -> String
stripTag msg = take (findSubstr " #sttm" msg) msg

readTweet :: String -> (Id, Thunk)
readTweet s =
  let up_id : spine_parts = words s
      (up, id) = splitAt 4 up_id
      spine = map readAtom spine_parts
      evaled = up == "#upd"
  in (id, (spine, evaled))

showSpine :: Spine -> String
showSpine spine = intercalate " " (map show spine)

showThunk :: Id -> Thunk -> String
showThunk id (spine, updated) =
  "#" ++ op ++ id ++ " " ++ intercalate " " (map show spine)
  where op = if updated then "upd" else "new"

makeStringHash :: String -> String
makeStringHash =
  read . show . Base64.encode . BS.concat . BL.toChunks . Binary.encode . unabcd . md5 . Str
  where unabcd (ABCD a) = a


class Monad m => TweetMachine m where
  nameThunk :: Spine -> m Id
  publishThunk :: Id -> Thunk -> m ()
  pullThunk :: Id -> m Spine

type LTM = State (Int, M.Map Id Thunk)
runLTM :: LTM a -> a
runLTM m = evalState m (0, M.empty)
instance TweetMachine LTM where
  nameThunk spine = do
    (n, m) <- get
    put (n+1, m)
    return (show n)

  publishThunk id thunk = do
    (n, m) <- get
    let m' = M.insert id thunk m
    put (n, m')

  pullThunk id = do
    (n, m) <- get
    let (spine, evaluated) = ((M.!) m id)
    if evaluated then return spine
      else do
      spine' <- evalSpine spine
      updateThunk id spine'
      return spine'


type STTM = ReaderT (String, MVar [String]) IO
runSTTM :: String -> MVar [String] -> STTM a -> IO a
runSTTM login mvar m = runReaderT m (login, mvar)

searchTweets :: String -> STTM [String]
searchTweets s = do
  (_, mvar) <- ask
  tweets <- liftIO $ readMVar mvar
  return $ filter (isInfixOf s) tweets

sendTweet :: String -> STTM ()
sendTweet msg = do
  (login, _) <- ask
  time <- liftIO getCurrentTime
  let msg' = msg ++ " #sttm " ++ takeWhile (/='.') (show (utctDayTime time))
  liftIO $ putStrLn $ "tweeting: " ++ msg'
  liftIO $ tweet login msg'
  return ()

instance TweetMachine STTM where
  nameThunk spine = do
    return $ take 4 $ makeStringHash $ showSpine spine

  publishThunk id thunk = sendTweet (showThunk id thunk)

  pullThunk id = do
    let loop first_time = do
          answers <- searchTweets ("#upd" ++ id)
          case answers of
            [] -> do
              when first_time $ sendTweet ("#eval" ++ id)
              liftIO (threadDelay 100000)
              loop False
            msg : _ ->
              let (_, (spine, _)) = readTweet $ stripTag msg
              in return spine
    loop True

updateThunk :: TweetMachine m => Id -> Spine -> m ()
updateThunk id spine = publishThunk id (spine, isWHNF spine)

newThunk :: TweetMachine m => Spine -> m Id
newThunk spine = do
  id <- nameThunk spine
  updateThunk id spine
  return id

spineify :: K.Expr -> [K.Expr]
spineify (K.App e1 e2) = spineify e1 ++ [e2]
spineify e = [e]

flattenExpr :: TweetMachine m => K.Expr -> m Atom
flattenExpr (e @ (K.App _ _)) = do
  spine <- mapM flattenExpr (spineify e)
  id <- newThunk spine
  return $ Thunk id
flattenExpr K.S = return S
flattenExpr K.K = return K
flattenExpr K.I = return I

stepSpine :: TweetMachine m => Spine -> m Spine
stepSpine (I:e:es) = return $ e : es
stepSpine (K:e1:e2:es) = return $ e1 : es
stepSpine (S:e1:e2:e3:es) = do
  id1 <- newThunk [e1, e3]
  id2 <- newThunk [e2, e3]
  return $ Thunk id1 : Thunk id2 : es
stepSpine (Inc:e:es) = return $ e : Inc : es
stepSpine (Num n:Inc:es) = return $ Num (n+1) : es
stepSpine (Thunk id:es) = do
  spine <- pullThunk id
  return $ spine ++ es
stepSpine spine = return spine

isWHNF :: Spine -> Bool
isWHNF (I:e:es) = False
isWHNF (K:e1:e2:es) = False
isWHNF (S:e1:e2:e3:es) = False
isWHNF (Inc:e:es) = False
isWHNF (Num n:Inc:es) = False
isWHNF (Thunk id:es) = False
isWHNF spine = True


evalSpine :: TweetMachine m => Spine -> m Spine
evalSpine spine = do
  spine' <- stepSpine spine
  if isWHNF spine' then return spine' else evalSpine spine'

flattener :: K.Expr -> (Atom, M.Map Id Thunk)
flattener e = 
  let (atom, (n, map)) = runState (flattenExpr e) (0 :: Int, M.empty)
  in (atom, map)

runCombProgram :: TweetMachine m => K.Expr -> m [Int]
runCombProgram comb = do
  root <- flattenExpr (K.App comb K.I)
  cdr_id <- newThunk [K, I]  
  let run a = do
        [Num n] <- evalSpine (value a)
        id <- newThunk (cdr a)
        rest <- run (Thunk id)
        return $ n : rest
      value a = [a, K, Inc, Num 0]
      cdr a = [a, Thunk cdr_id]
    in run root

evalCombNumber :: TweetMachine m => K.Expr -> m Int
evalCombNumber comb = do
  root <- flattenExpr comb
  id <- newThunk [root, Inc, Num 0]
  [Num n] <- pullThunk id
  return n

sttmMsgThread :: String -> STTM ()
sttmMsgThread s =  do
  when ("#eval" `isPrefixOf` s) $ do
    let id = drop 5 s
    thunks <- searchTweets $ "#new" ++ id
    case thunks of
      [] -> return ()
      thunk_msg : _ -> do
        let (_, (spine, _)) = readTweet $ stripTag thunk_msg
        spine' <- evalSpine spine
        updateThunk id spine'

monitorStream :: Bool -> MVar [String] -> String -> IO ()
monitorStream isWorker mvar id = do
  (stdin, stdout, stderr, proc) <-
    runInteractiveProcess "curl"
    ["-s", "-N", "-d", "@twittering/following",
     "https://stream.twitter.com/1/statuses/filter.json",
     "-usttm" ++ id ++ ":buttsquid"] Nothing Nothing
  hSetBinaryMode stdout False
  loop stdout
  where loop h = do
          line <- hGetLine h
          handleMsg (JS.decode line)
          loop h
        handleMsg (JS.Error _) = return ()
        handleMsg (JS.Ok (JS.JSObject obj)) = do
          let JS.JSString smsg = fromJust $ lookup "text" $ JS.fromJSObject obj
          let msg = JS.fromJSString smsg
          contents <- takeMVar mvar
          putMVar mvar (msg : contents)
          if " #sttm" `isInfixOf` msg then
            when isWorker $ processSttmMsg msg else
            putStrLn $ "got nonsense: " ++ msg
        processSttmMsg msg = do
          forkIO (runSTTM id mvar (sttmMsgThread (stripTag msg)))
          return ()

twitterLogin :: String -> IO ExitCode
twitterLogin user =
  rawSystem "./twittering/login.sh" [user]

tweet :: String -> String -> IO ExitCode
tweet user message =
  rawSystem "./twittering/tweet.sh" [user, encodeString message]

ltm_main_program = do
  [sourceFile] <- getArgs
  source <- readFile sourceFile
  let comb = K.parse source
  mapM_ K.outputCharacter $ runLTM (runCombProgram comb)

ltm_main = do
  [sourceFile] <- getArgs
  source <- readFile sourceFile
  let comb = K.parse source
  let answer = runLTM $ evalCombNumber comb
  putStrLn (show answer)

sttm_main = do
  login : restArgs <- getArgs
  let isWorker = null restArgs
  tweetListMvar <- newMVar []
  twitterLogin login
  let monitor = monitorStream isWorker tweetListMvar login
  if isWorker then monitor else do
    forkIO monitor
    source <- readFile (head restArgs)
    let comb = K.parse source
    answer <- runSTTM login tweetListMvar $ evalCombNumber comb
    putStrLn (show answer)

main = sttm_main
