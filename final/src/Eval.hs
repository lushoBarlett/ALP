{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval(eval, EvalT(..), defaultRunEnv, defaultState, run) where

import Common (QC(..), State(..), Environment(..), Matrix(..), tensorQBit, qbitFromNumber, tensor, tensoreye, Operator, Name)
import Control.Monad.State (MonadState(..), StateT(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)
import Data.Complex (Complex(..))
import qualified Data.Map as Map
import Data.List (elemIndex)

newtype EvalT a = EvalT {
  runEvalT :: ReaderT Environment (StateT State (ExceptT String IO)) a
} deriving (Functor, Applicative, Monad, MonadReader Environment, MonadState State, MonadError String)

run :: EvalT a -> Environment -> State -> IO (Either String (a, State))
run evalM initEnv initState =
  let evaluator = runEvalT evalM
      withInitialEnv = runReaderT evaluator initEnv
      withInitialState = runStateT withInitialEnv initState
  in
    runExceptT withInitialState

defaultRunEnv :: Environment
defaultRunEnv = Environment {
  circuits = Map.empty,
  gates = Map.fromList [
    ("I", QCI),
    ("X", QCX),
    ("Y", QCY),
    ("Z", QCZ),
    ("H", QCH)
  ]
}

defaultState :: State
defaultState = State {
  qbits = Matrix 1 1 [1],
  qbitnames = []
}

updateState :: (State -> State) -> EvalT ()
updateState f = do
  s <- get
  put $ f s

-- conceputalmente lo que tenemos es
-- Q' subset de Q
-- podemos computar una matriz A aplicable a Q'
-- pero no siempre podemos expandir A a una matriz B aplicable a Q
-- pues el producto tensorial no es conmutativo
-- por ejemplo, si Q = [q1, q2, q3, q4] y Q' = [q1,q3], entonces si A debe ser
-- computada a partir de una compuerta definida por el usuario, estamos al horno

-- lo correcto sería hacer los swaps de qbits necesarios para que Q' sea un
-- prefijo de Q, y luego aplicar la matriz A tensorial con las identidades.

-- Ineficiente, pero correcto.

-- La otra es expandir la compuerta... pero no está buena esa idea me parece

-- swap i j n es la matriz que cambia las siguientes columnas
-- |..2^i..2^j..> -> |..2^j..2^i..>
-- |.. 1 .. 0 ..> -> |.. 0 .. 1 ..>
-- |.. 0 .. 1 ..> -> |.. 1 .. 0 ..>
-- |v> -> |v>
swap :: Int -> Int -> Int -> Operator

swap i j n | i > j     = swap j i n

swap i j n = Matrix p p $ concat [row k | k <- indices]
  where
    p = 2 ^ n
    indices = [0 .. p - 1]

    row k | bitOn k i && bitOff k j = row' (k + 2 ^ j - 2 ^ i)
          | bitOff k i && bitOn k j = row' (k - 2 ^ j + 2 ^ i)
          | otherwise  = row' k

    row' k = [if z == k then 1 else 0 | z <- indices]

    bitOn k b = (k `div` 2 ^ b) `mod` 2 == 1
    bitOff k b = not $ bitOn k b

slice :: Int -> Int -> [a] -> [a]
slice i j xs = take (j - i) $ drop i xs

putIth :: Int -> Name -> [Name] -> ([Name], Operator)
putIth i name names = case elemIndex name names of
  Nothing -> error $ "putIth: name not found " ++ name ++ " in " ++ show names
  Just j -> if i < j
    then (take (i - 1) names ++ jth ++ slice (i+1) j names ++ ith ++ drop (j + 1) names, sm)
    else (take (j - 1) names ++ ith ++ slice (j+1) i names ++ jth ++ drop (i + 1) names, sm)
      where
        n = length names
        sm = swap i j n
        ith = [names !! i]
        jth = [names !! j]

-- move the qbits involved in an operation to the front of the list, and return the swap operator
swapToPrefix :: [(Int,Name)] -> [Name] -> ([Name], Operator)
swapToPrefix [] allnames = (allnames, tensoreye $ length allnames)
swapToPrefix ((i,name):names) allnames = (finalnames', sm' <> sm) -- flipped
  where
    (allnames', sm) = putIth i name allnames
    (finalnames', sm') = swapToPrefix names allnames'

expand :: [Name] -> [Operator] -> EvalT Operator
expand names partialOps = do
  s <- get

  let allnames = qbitnames s

  let (_, swapop) = swapToPrefix (zip [0..] names) allnames

  -- tensor with leftover identities
  let leftover = tensoreye (length allnames - length names)

  let op = foldr1 tensor (partialOps ++ [leftover])

  -- swap before and after you apply the operatoz
  return $ swapop <> op <> swapop

resolveVariable :: Name -> EvalT QC
resolveVariable name = do
  env <- ask
  case Map.lookup name (gates env) of
    Nothing -> throwError $ "Variable " ++ name ++ " not found (in global scope, local is not implemented)"
    Just qc -> return qc

-- NOTE: because '->' is basically flipped matrix multiplication,
-- we flip several things here to make it work.

eval :: QC -> EvalT ()
eval (QCCircuit _ preps body) = mapM_ eval preps >> evalSeq body
eval (QCPreparation n name) = updateState $ \s -> tensorQBit s name $ qbitFromNumber n
eval _ = undefined

evalSeq :: [QC] -> EvalT ()
evalSeq [] = return ()
evalSeq ((QCOperation names qc):qcs) = do
  op <- evalOperator names qc
  updateState $ \s -> s {
    -- flipped
    qbits = op <> qbits s
  }
  evalSeq qcs
evalSeq (gate@(QCGate name _ _):qcs) =
  local (\env -> env { gates = Map.insert name gate (gates env) }) $ evalSeq qcs
evalSeq (qc:_) = error $ "Not implemented: evalSeq for " ++ show qc

evalOperator :: [Name] -> QC -> EvalT Operator
evalOperator names (QCArrow qc1 qc2) = do
  op1 <- evalOperator names qc1
  op2 <- evalOperator names qc2
  -- flipped
  return $ op2 <> op1

evalOperator names (QCTensors tensors) = do
  partialOps <- evalTensors names tensors
  expand names partialOps

evalOperator names (QCVariable name) = resolveVariable name >>= evalOperator names

evalOperator _ (QCOperation names qc) = evalOperator names qc
evalOperator _ QCI = return $ Matrix 2 2 [1, 0, 0, 1]
evalOperator _ QCX = return $ Matrix 2 2 [0, 1, 1, 0]
evalOperator _ QCY = return $ Matrix 2 2 [0, 0 :+ (-1), 0 :+ 1, 0]
evalOperator _ QCZ = return $ Matrix 2 2 [1, 0, 0, -1]
evalOperator _ QCH = return $ Matrix 2 2 [1 / sqrt 2, 1 / sqrt 2, 1 / sqrt 2, -1 / sqrt 2]
evalOperator _ (QCGate _ args body) = compileOperator args body
evalOperator _ qc = error $ "Not implemented: evalOperator for " ++ show qc

compileOperator :: [Name] -> [QC] -> EvalT Operator
compileOperator args body = foldl1 (<>) <$> reversedChain
  where
    evalChain = sequenceA (evalOperator args <$> body)
    -- flipped
    reversedChain = reverse <$> evalChain

mismatch :: EvalT a
mismatch = throwError "Qbit/Operator mismatch"

evalTensors :: [Name] -> [QC] -> EvalT [Operator]
evalTensors []    []                          = return []
evalTensors (_:_) []                          = mismatch
evalTensors names ((QCVariable name):tensors) = resolveVariable name >>= evalTensors names . (:tensors)
evalTensors names (t:tensors)                 = do
  let n = arguments t
  if n > length names
    then mismatch
    else do
      op <- evalOperator (take n names) t
      ops <- evalTensors (drop n names) tensors
      return $ op : ops

arguments :: QC -> Int
arguments (QCGate _ names _) = length names
arguments QCI = 1
arguments QCX = 1
arguments QCY = 1
arguments QCZ = 1
arguments QCH = 1
arguments qc = error $ "Not implemented: arguments for " ++ show qc