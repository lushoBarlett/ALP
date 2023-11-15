module Eval3
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

-- Ejercicio 3.a: Proponer una nueva m\'onada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 
newtype StateErrorTrace a =
  StateErrorTrace { runStateErrorTrace :: Env -> Either Error (Pair (a, String) Env)}

instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return
  (<*>) = ap

instance Monad StateErrorTrace where
  return x = StateErrorTrace $ \env -> Right ((x, "") :!: env)
  m >>= f = StateErrorTrace $ \env -> case runStateErrorTrace m env of
    Left e -> Left e
    Right ((x, t) :!: env') -> case runStateErrorTrace (f x) env' of
      Left e -> Left e
      Right ((y, t') :!: env'') -> Right ((y, t ++ t') :!: env'')

-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  trace s = StateErrorTrace $ \env -> Right $ ((), s) :!: env

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = StateErrorTrace $ \_ -> Left e

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = StateErrorTrace $ \env -> case M.lookup v env of
    Nothing -> Left UndefVar
    Just x  -> Right $ (x, "") :!: env
  update v x = StateErrorTrace $ \env -> Right $ ((), "") :!: M.insert v x env

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Env, Trace)
eval comm = case runStateErrorTrace (stepCommStar comm) initEnv of
  Left e -> Left e
  Right ((_, t) :!: env) -> Right (env, t)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm Skip = return Skip

stepComm (Let v e) = do
  x <- evalExp e
  update v x
  trace $ v ++ " = " ++ show x ++ "\n"

stepComm (Seq Skip c2) = return c2

stepComm (Seq c1 c2) = do
  c1' <- stepComm c1
  return $ Seq c1' c2

stepComm (IfThenElse b c1 c2) = do
  b' <- evalExp b
  if b' then return c1 else return c2

stepComm r@(While bexp c) = return $ IfThenElse bexp r Skip

evalUnOp :: Monad m => (a -> b) -> Exp a -> m b
evalUnOp f e = do -- esto no es fmap?
  v <- evalExp e
  return (f v)

evalBinOp :: Monad m => (a -> b -> c) -> Exp a -> Exp b -> m c
evalBinOp f e1 e2 = do -- esto no es ap?
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (f v1 v2)

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
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
  s <- get
  put $ update v n s
  trace $ v ++ " = " ++ show n ++ "\n"
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
