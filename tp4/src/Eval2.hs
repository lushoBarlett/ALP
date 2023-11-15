module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error (Pair a Env) }

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError $ \env -> Right (x :!: env)
  m >>= f = StateError $ \env -> case runStateError m env of
    Left e -> Left e
    Right (x :!: env') -> runStateError (f x) env'

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw e = StateError $ \_ -> Left e

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError $ \env -> case M.lookup v env of
    Nothing -> Left UndefVar
    Just x  -> Right (x :!: env)
  update v x = StateError $ \env -> Right (() :!: M.insert v x env)

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval comm = case runStateError (stepCommStar comm) initEnv of
  Left e -> Left e
  Right (_ :!: env) -> Right env

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm Skip = return Skip

stepComm (Let var exp) = do
  v <- evalExp exp
  update var v
  return Skip

stepComm (Seq Skip c2) = return c2
stepComm (Seq c1 c2) = do
  c1' <- stepComm c1
  return (Seq c1' c2)

stepComm (IfThenElse b c1 c2) = do
  b' <- evalExp b
  if b' then return c1 else return c2

stepComm r@(While bexp c) = return $ IfThenElse bexp r Skip

evalUnOp :: (MonadState m, MonadError m) => (a -> b) -> Exp a -> m b
evalUnOp f e = do
  v <- evalExp e
  return (f v)

evalBinOp :: (MonadState m, MonadError m) => (a -> b -> c) -> Exp a -> Exp b -> m c
evalBinOp f e1 e2 = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (f v1 v2)

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
-- enteras
evalExp (Const n) = return n
evalExp (Var x) = lookfor x
evalExp (UMinus e) = evalUnOp negate e
evalExp (Plus e1 e2) = evalBinOp (+) e1 e2
evalExp (Minus e1 e2) = evalBinOp (-) e1 e2
evalExp (Times e1 e2) = evalBinOp (*) e1 e2
evalExp (Div e1 e2) = do
  v2 <- evalExp e2
  if v2 == 0 then throw DivByZero else evalBinOp div e1 e2
evalExp (EAssgn v e) = do
  n <- evalExp e
  update v n
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
