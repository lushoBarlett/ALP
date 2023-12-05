{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval(eval, EvalT(..), defaultRunEnv, defaultState, run) where

import Common (QC(..), State(..), Environment(..), tensorQBit, tensoreye, Operator, Name, linearTransformation, castFromInt, castFromReal, addGate)
import Matrix (Matrix(..), RowMatrix(..), fromRowToCol, ColMatrix (ColMatrix))
import QBit (qbitFromNumber, toColMatrix, QBitBase (..))
import Control.Monad.State (MonadState(..), StateT(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)
import Data.Complex (Complex(..))
import qualified Data.Map as Map
import Data.List (elemIndex)
import Data.Bits (testBit, setBit, clearBit, Bits (..))
import Debug.Trace (traceM)

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
  qbits = fromRowToCol $ RowMatrix 1 1 [1],
  qbitnames = []
}

updateState :: (State -> State) -> EvalT ()
updateState f = do
  s <- get
  put $ f s

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

renumerate :: Int -> [a] -> [(Int, a)]
renumerate n = zip $ reverse [0..n-1]

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
swap i j n = castFromInt $ linearTransformation n f
  where
    f base | testBit base i && not (testBit base j) = toColMatrix $ setBit (clearBit base i) j
           | not (testBit base i) && testBit base j = toColMatrix $ setBit (clearBit base j) i
           | otherwise = toColMatrix base

slice :: Int -> Int -> [a] -> [a]
slice i j xs = take (j - i) $ drop i xs

putIth :: Int -> Name -> [Name] -> ([Name], Operator)
putIth i name names = case elemIndex name names of
  Nothing -> error $ "putIth: name not found " ++ name ++ " in " ++ show names
  Just j -> if i == j
    then (names, castFromInt $ tensoreye n)
    else if i < j
      then (take i names ++ jth ++ slice (i + 1) j names ++ ith ++ drop (j + 1) names, sm)
      else (take j names ++ ith ++ slice (j + 1) i names ++ jth ++ drop (i + 1) names, sm)
    where
      n = length names
      -- leftmost qbit is the most significant means its index is its complement significance
      -- because swap works with qbit significances, we complement the indices
      sm = swap (n - i - 1) (n - j - 1) n
      ith = [names !! i]
      jth = [names !! j]

-- move the qbits involved in an operation to the front of the list, and return the swap operator
swapToPrefix :: [(Int,Name)] -> [Name] -> ([Name], Operator)
swapToPrefix [] allnames = (allnames, castFromInt $ tensoreye $ length allnames)
swapToPrefix ((i,name):names) allnames = (finalnames', sm' <> sm) -- flipped
  where
    (allnames', sm) = putIth i name allnames
    (finalnames', sm') = swapToPrefix names allnames'

expand :: [Name] -> [Operator] -> EvalT Operator
expand names partialOps = do
  s <- get

  let allnames = qbitnames s

  let (_, swapop) = swapToPrefix (enumerate names) allnames
  let swapinv = transpose swapop

  -- tensor with leftover identities
  let leftover = castFromInt $ tensoreye (length allnames - length names)

  let op = foldr1 tensor (partialOps ++ [leftover])

  -- swap before and after you apply the operator
  return $ swapinv <> op <> swapop

resolveVariable :: Name -> EvalT QC
resolveVariable name = do
  env <- ask
  case Map.lookup name (gates env) of
    Nothing -> throwError $ "Variable " ++ name ++ " not found (in global scope, local is not implemented)"
    Just qc -> return qc

-- NOTE: because '->' is basically flipped matrix multiplication,
-- we flip several things here to make it work.

operate :: Operator -> EvalT ()
operate op = updateState $ \s -> s { qbits = op <> qbits s } -- flipped

variables :: QC -> [Name]
variables (QCCircuit _ _ body) = concatMap variables body
variables (QCIf conditions body) = concatMap variables conditions ++ concatMap variables body
variables (QCVariable name) = [name]
variables (QCNegatedVariable name) = [name]
variables (QCOperation names _) = names
variables _ = []

someOccursIn :: [Name] -> [Name] -> Bool
someOccursIn [] _ = False
someOccursIn (x:xs) ys = x `elem` ys || xs `someOccursIn` ys

eval :: QC -> EvalT ()
eval (QCCircuit _ preps body) = mapM_ eval preps >> evalSeq body
eval (QCPreparation n name) = updateState $ \s -> tensorQBit s name $ castFromInt $ toColMatrix $ qbitFromNumber n
eval _ = error "Not implemented: eval"

-- conceputalmente lo que tenemos es
-- Q' subset de Q
-- donde Q' son las variables condiciones y Q\Q' es el resto
-- queremos aplicar un operador a Q\Q' solamente si se cumplen las condiciones de Q'

-- hacemos swap para poner las condiciones al frente de Q, y luego creamos
-- una transformacion lineal que transforma un vector canonico con (I `tensor` op)
-- si la parte de las condiciones matchea exáctamente, y sino es (I `tensor` I)
-- tener en cuenta que lo construimos por columnas.

-- el operador se computa compilando el cuerpo del if, que es multiplicar
-- los efectos de cada linea del cuerpo, con las expansiones adecuadas

-- |cond,rest> -> (IxH)|cond,rest> (si matchea)
-- |v> -> |v> (en otro caso)
compileIf :: [QC] -> [QC] -> EvalT Operator
compileIf conditions body = do
  s <- get

  let vs = concatMap variables conditions

  let allnames = qbitnames s

  let conditionsBits = toBase (length allnames)

  let (allnames', swapop) = swapToPrefix (enumerate vs) allnames
  let swapinv = transpose swapop

  put $ s { qbitnames = remove vs allnames' } -- ! FIXME: ugly hack, `expand` reads the state for the names
  op <- compileOperator (remove vs allnames') body -- remove variables from conditions, they can't be used
  put s -- restore

  let leftover = castFromInt $ tensoreye $ length vs

  let fullop = leftover `tensor` op

  -- apply op conditionally, according to the conditions
  let op' = linearTransformation (length allnames) f
        where
          f base | matchesConditions conditionsBits base = ColMatrix (2^tensorDimension base) 1 $ col (baseValueRepr base) fullop
                 | otherwise = toColMatrix base -- id

  return $ swapinv <> op' <> swapop

  where
    toBits = map $ uncurry varToBit

    varToBit k (QCVariable _) = bit k
    varToBit _ (QCNegatedVariable _) = 0
    varToBit _ _ = error "Not implemented: varToCond"

    toBase n = QBitBase (sum $ toBits $ renumerate n conditions) n

    remove vs = drop (length vs) -- all at the front

    matchesConditions bits base = all (\i -> testBit base i == testBit bits i) [b..tensorDimension bits - 1]
      where b = tensorDimension bits - length conditions -- start from here, lower significance bits are not used


evalSeq :: [QC] -> EvalT ()
evalSeq [] = return ()
evalSeq ((QCOperation names qc):qcs) = evalOperator names qc >>= operate >> evalSeq qcs
evalSeq (gate@(QCGate name _ _):qcs) = local (addGate name gate) $ evalSeq qcs
evalSeq ((QCIf conditions body):qcs) = do
  let vs = concatMap variables conditions
  let vs' = concatMap variables body
  if vs `someOccursIn` vs'
    then throwError "Variables in if condition must not occur in body"
    else compileIf conditions body >>= operate >> evalSeq qcs

evalSeq (qc:_) = error $ "Not implemented: evalSeq for " ++ show qc

evalOperator :: [Name] -> QC -> EvalT Operator
evalOperator _ (QCOperation names qc) = evalOperator names qc

evalOperator names (QCArrow qc1 qc2) = do
  op1 <- evalOperator names qc1
  op2 <- evalOperator names qc2
  -- flipped
  return $ op2 <> op1

evalOperator names (QCTensors tensors) = do
  partialOps <- evalTensors names tensors
  expand names partialOps

evalOperator names (QCVariable name) = resolveVariable name >>= evalOperator names

evalOperator _ QCI = return $  castFromInt $ fromRowToCol $ RowMatrix 2 2 [1, 0, 0, 1]
evalOperator _ QCX = return $  castFromInt $ fromRowToCol $ RowMatrix 2 2 [0, 1, 1, 0]
evalOperator _ QCY = return $                fromRowToCol $ RowMatrix 2 2 [0, 0 :+ (-1), 0 :+ 1, 0]
evalOperator _ QCZ = return $  castFromInt $ fromRowToCol $ RowMatrix 2 2 [1, 0, 0, -1]
evalOperator _ QCH = return $ castFromReal $ fromRowToCol $ RowMatrix 2 2 [1 / sqrt 2, 1 / sqrt 2, 1 / sqrt 2, -1 / sqrt 2]
evalOperator _ (QCGate _ args body) = do
  s <- get
  put $ s { qbitnames = args } -- ! FIXME: ugly hack, `expand` reads the state for the names
  op <- compileOperator args body
  put s -- restore
  return op

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
