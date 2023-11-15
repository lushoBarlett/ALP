module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval comm = snd $ runState (stepCommStar comm) initEnv

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm

stepComm Skip = return Skip

stepComm (Let v e) = do
  n <- evalExp e
  s <- get
  put $ update v n s
  return Skip

stepComm (Seq Skip c2) = return c2
stepComm (Seq c1 c2) = do
  c1' <- stepComm c1
  return $ Seq c1' c2

stepComm (IfThenElse bexp c1 c2) = do
  b <- evalBExp bexp
  if b then return c1 else return c2

stepComm r@(While bexp c) = return $ IfThenElse bexp r Skip

evalUnOp :: MonadState m => (a -> b) -> Exp a -> m b
evalUnOp f e = do
  n <- evalExp e
  return $ f n

evalBinOp :: MonadState m => (a -> b -> c) -> Exp a -> Exp b -> m c
evalBinOp f e1 e2 = do
  n1 <- evalExp e1
  n2 <- evalExp e2
  return $ f n1 n2

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
-- enteras
evalExp (Const n) = return n
-- lookfor ?
evalExp (Var v) = do
  s <- get
  return $ Maybe.fromMaybe undefined (M.lookup v s)
evalExp (UMinus e) = evalUnOp negate e
evalExp (Plus e1 e2) = evalBinOp (+) e1 e2
-- asociatividad?
evalExp (Minus e1 e2) = evalBinOp (-) e1 e2
evalExp (Times e1 e2) = evalBinOp (*) e1 e2
evalExp (Div e1 e2) = evalBinOp div e1 e2
evalExp (EAssgn v e) = do
  n <- evalExp e
  s <- get
  put $ update v n s
  return n
evalExp (ESeq e1 e2) = do
  evalExp e1
  evalExp e2
-- booleanas
evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt e1 e2) = evalBinOp (<) e1 e2
evalExp (Gt e1 e2) = evalBinOp (>) e1 e2
evalExp (And e1 e2) = evalBinOp (&&) e1 e2
evalExp (Or e1 e2) = evalBinOp (||) e1 e2
evalExp (Not e) = evalUnOp not e
evalExp (Eq e1 e2) = evalBinOp (==) e1 e2
evalExp (NEq e1 e2) = evalBinOp (/=) e1 e2
