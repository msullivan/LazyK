{-#LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Main
(main
) where 

import qualified LazyK as K
import Data.Array
import Data.Char
import Data.List
import System.Exit
import System(getArgs)
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

import Web.Codec.URLEncoder
import Web.Twitter hiding (tweet)
--import Web.Twitter.Post
import Web.Twitter.Fetch
import Web.Twitter.Monad hiding (liftIO)
import Web.Twitter.Types

import System.Process

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

showUpdate :: Id -> Thunk -> String
showUpdate id (spine, updated) =
  "#" ++ op ++ id ++ " " ++ intercalate " " (map show spine) ++ " #sttm"
  where op = if updated then "u" else "n"

class Monad m => TweetMachine m where
  nameThunk :: Spine -> m Id
  publishThunk :: Id -> Thunk -> m ()
  pullThunk :: Id -> m Spine

type LTM = State (Int, M.Map Id Thunk)
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


type STTM = StateT (Int, M.Map Id Thunk) IO
instance TweetMachine STTM where
  nameThunk spine = do
    (n, m) <- get
    put (n+1, m)
    return (show n)

  publishThunk id thunk = do
    (n, m) <- get
    let m' = M.insert id thunk m
    liftIO $ putStrLn (showUpdate id thunk)
    --liftIO $ tweet "0001" (showUpdate id thunk)
    put (n, m')

  pullThunk id = do
    (n, m) <- get
    let (spine, evaluated) = ((M.!) m id)
    if evaluated then return spine
      else do
      spine' <- evalSpine spine
      updateThunk id spine'
      return spine'


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

runLTM :: LTM a -> a
runLTM m = evalState m (0, M.empty)

runSTTM :: STTM a -> IO a
runSTTM m = evalStateT m (0, M.empty)


twitterLogin :: String -> IO ExitCode
twitterLogin user =
  rawSystem "./twittering/login.sh" [user]

tweet :: String -> String -> IO ExitCode
tweet user message =
  rawSystem "./twittering/tweet.sh" [user, encodeString message]
runTweet = runTM (AuthUser {authUserName = "", authUserPass = "" })

userSearch :: String -> IO [String]
userSearch user = do
  result <- runTweet (getUserTimeline (Just user) Nothing Nothing)
  return $ map statusText result

main2 :: IO ()
main2 = do
  twitterLogin "0001"
  --tweet "0001" "argh testing! #sttm"
  --let search_ctx = searchFor { searchHashTag = "sttm" }
  --result <- runTweet (search search_ctx)
  statuses <- userSearch "sttm0001"
  mapM_ putStrLn statuses
  --putStrLn (show result)
  return ()


main = do
  [sourcePath] <- getArgs
  --twitterLogin "0001"
  source <- readFile sourcePath
  let comb = K.parse source
  --mapM_ K.outputCharacter $ runLTM (runCombProgram comb)
  answer <- runSTTM $ evalCombNumber comb
  putStrLn (show answer)
