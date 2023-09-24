module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = (M.Map Variable Int, String)

-- Estado nulo
-- Completar la definición
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v (m, t) = case M.lookup v m of
  Just x  -> Right x
  Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v i s =
  let (m, t) = s
   in (M.insert v i m, t)

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace s state =
  let (m, t) = state
   in (m, t ++ s ++ "\n")

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)

stepComm Skip s = return $ Skip :!: s

stepComm (Let v e) s = do
  n :!: s' <- evalExp e s
  let (m, t) = s'
      t' = "Let " ++ v ++ " " ++ show n ++ "\n"
   in return $ Skip :!: (M.insert v n m, t ++ t')

stepComm (Seq Skip c2) s = return $ c2 :!: s
stepComm (Seq c1 c2) s = do
  c1' :!: s' <- stepComm c1 s
  return $ (Seq c1' c2) :!: s'

stepComm (IfThenElse bexp c1 c2) s = do
  b :!: s' <- evalExp bexp s
  return $ if b then c1 :!: s' else c2 :!: s'

stepComm (Repeat c bexp) s = return $ newcomm :!: s
  where
    newcomm = (Seq c (IfThenElse bexp Skip repeat))
    repeat = Repeat c bexp

-- Evalua una expresion
-- Completar la definición
evalUnOp :: (a -> b) -> Exp a -> State -> Either Error (Pair b State)
evalUnOp op e s = do
  n :!: s' <- evalExp e s
  return $ (op n) :!: s'

evalBinOp :: (a -> b -> c) -> Exp a -> Exp b -> State -> Either Error (Pair c State)
evalBinOp op e1 e2 s = do
  n1 :!: s' <- evalExp e1 s
  n2 :!: s'' <- evalExp e2 s'
  return $ (op n1 n2) :!: s''

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
-- enteras
evalExp (Const n) s     = return $ n :!: s
evalExp (Var v) s       = do
  n <- lookfor v s
  return $ n :!: s
evalExp (UMinus e) s    = evalUnOp negate e s
evalExp (Plus e1 e2) s  = evalBinOp (+) e1 e2 s
evalExp (Minus e1 e2) s = evalBinOp (-) e1 e2 s
evalExp (Times e1 e2) s = evalBinOp (*) e1 e2 s
evalExp (Div e1 e2) s   = do
  n2 :!: s' <- evalExp e2 s
  if n2 == 0
    then Left DivByZero
    else evalBinOp div e1 e2 s
-- ! FIXME: Falta un trace acá
evalExp (EAssgn v e) s  = do
  n :!: s' <- evalExp e s
  let s'' = update v n s'
   in return $ n :!: s''
evalExp (ESeq e1 e2) s  = do
  _ :!: s'  <- evalExp e1 s
  n :!: s'' <- evalExp e2 s'
  return $ n :!: s''
-- booleanas
evalExp BTrue s       = return $ True :!: s
evalExp BFalse s      = return $ False :!: s
evalExp (Lt e1 e2) s  = evalBinOp (<) e1 e2 s
evalExp (Gt e1 e2) s  = evalBinOp (>) e1 e2 s
evalExp (And e1 e2) s = evalBinOp (&&) e1 e2 s
evalExp (Or e1 e2) s  = evalBinOp (||) e1 e2 s
evalExp (Not e) s     = evalUnOp not e s
evalExp (Eq e1 e2) s  = evalBinOp (==) e1 e2 s
evalExp (NEq e1 e2) s = evalBinOp (/=) e1 e2 s
