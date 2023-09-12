module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = case M.lookup v s of
  Just x  -> x
  Nothing -> undefined

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State

stepComm Skip s = Skip :!: s

stepComm (Let v e) s =
  let n :!: s' = evalExp e s
   in Skip :!: (update v n s')

stepComm (Seq Skip c2) s = c2 :!: s
stepComm (Seq c1 c2) s =
  let c1' :!: s' = stepComm c1 s
   in (Seq c1' c2) :!: s'

stepComm (IfThenElse bexp c1 c2) s =
  let b :!: s' = evalExp bexp s
   in if b then c1 :!: s' else c2 :!: s'

stepComm (Repeat c bexp) s = newcomm :!: s
  where
    newcomm = (Seq c (IfThenElse bexp Skip repeat))
    repeat = Repeat c bexp

-- Evalua una expresion
-- Completar la definición
evalUnOp :: (a -> b) -> Exp a -> State -> Pair b State
evalUnOp op e s =
  let n :!: s' = evalExp e s
   in (op n) :!: s'

evalBinOp :: (a -> b -> c) -> Exp a -> Exp b -> State -> Pair c State
evalBinOp op e1 e2 s =
  let n1 :!: s' = evalExp e1 s
      n2 :!: s'' = evalExp e2 s'
   in (op n1 n2) :!: s''

evalExp :: Exp a -> State -> Pair a State
-- enteras
evalExp (Const n) s     = n :!: s
evalExp (Var v) s       = (lookfor v s) :!: s
evalExp (UMinus e) s    = evalUnOp negate e s
evalExp (Plus e1 e2) s  = evalBinOp (+) e1 e2 s
evalExp (Minus e1 e2) s = evalBinOp (-) e1 e2 s
evalExp (Times e1 e2) s = evalBinOp (*) e1 e2 s
evalExp (Div e1 e2) s   = evalBinOp div e1 e2 s
evalExp (EAssgn v e) s  =
  let n :!: s' = evalExp e s
      s'' = update v n s'
   in n :!: s''
evalExp (ESeq e1 e2) s  =
  let _ :!: s'  = evalExp e1 s
      n :!: s'' = evalExp e2 s'
   in n :!: s''
-- booleanas
evalExp BTrue s       = True :!: s
evalExp BFalse s      = False :!: s
evalExp (Lt e1 e2) s  = evalBinOp (<) e1 e2 s
evalExp (Gt e1 e2) s  = evalBinOp (>) e1 e2 s
evalExp (And e1 e2) s = evalBinOp (&&) e1 e2 s
evalExp (Or e1 e2) s  = evalBinOp (||) e1 e2 s
evalExp (Not e) s     = evalUnOp not e s
evalExp (Eq e1 e2) s  = evalBinOp (==) e1 e2 s
evalExp (NEq e1 e2) s = evalBinOp (/=) e1 e2 s
