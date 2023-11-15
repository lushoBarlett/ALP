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

instance Functor StateErrorCost where
  fmap = liftM

instance Applicative StateErrorCost where
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
eval = undefined

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
-- stepComm :: [dar el tipo segun corresponda]
stepComm = undefined

-- Evalua una expresion 
-- evalIntExp :: [dar el tipo segun corresponda]
evalExp = undefined
