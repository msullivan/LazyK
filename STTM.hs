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
import qualified Data.IntMap as M
import Data.Maybe

type Id = Int
data Atom = S | K | I | Inc | Num Int | Thunk Int
          deriving (Show, Ord, Eq)
type Spine = [Atom]
type Thunk = (Spine, Bool)

type LTM = State (Id, M.IntMap Thunk)
nameThunk :: Spine -> LTM Id
nameThunk spine = do
  (n, m) <- get
  put (n+1, m)
  return n
lookupThunk :: Id -> LTM Thunk
lookupThunk id = do
  (n, m) <- get
  return ((M.!) m id)
updateThunk :: Id -> Thunk -> LTM ()
updateThunk id thunk = do
  (n, m) <- get
  let m' = M.insert id thunk m
  put (n, m')
newThunk :: Spine -> LTM Id
newThunk spine = do
  id <- nameThunk spine
  updateThunk id (spine, False)
  return id

spineify :: K.Expr -> [K.Expr]
spineify (K.App e1 e2) = spineify e1 ++ [e2]
spineify e = [e]

flattenExpr :: K.Expr -> LTM Atom
flattenExpr (e @ (K.App _ _)) = do
  spine <- mapM flattenExpr (spineify e)
  id <- newThunk spine
  return $ Thunk id
flattenExpr K.S = return S
flattenExpr K.K = return K
flattenExpr K.I = return I

pullThunk :: Id -> LTM Spine
pullThunk id = do
  (spine, evaluated) <- lookupThunk id
  if evaluated then return spine
    else do
    spine' <- evalSpine spine
    updateThunk id (spine', True)
    return spine'

stepSpine :: Spine -> LTM Spine
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

evalSpine :: Spine -> LTM Spine
evalSpine spine = do
  spine' <- stepSpine spine
  if spine == spine' then return spine' else evalSpine spine'

flattener :: K.Expr -> (Atom, M.IntMap Thunk)
flattener e = 
  let (atom, (_, map)) = runState (flattenExpr e) (0, M.empty)
  in (atom, map)

runCombProgram :: K.Expr -> LTM [Int]
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

evalCombNumber :: K.Expr -> LTM Int
evalCombNumber comb = do
  root <- flattenExpr comb
  [Num n] <- evalSpine [root, Inc, Num 0]
  return n

runLTM :: LTM a -> a
runLTM m = evalState m (0, M.empty)

main :: IO ()
main = do
  [sourcePath] <- getArgs
  source <- readFile sourcePath
  let comb = K.parse source
  --mapM_ K.outputCharacter $ runLTM (runCombProgram comb)
  putStrLn (show (runLTM $ evalCombNumber comb))
  
