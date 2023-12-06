{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval(eval, EvalT(..), defaultRunEnv, defaultState, run) where

import Common (Environment (..), Name, Operator, QC (..), State (..),
               addGate, castFromInt, castFromReal, enumerate,
               linearTransformation, renumerate, tensorQBit,
               tensoreye)
import Matrix (Matrix(..), RowMatrix(..), fromRowToCol, ColMatrix (ColMatrix))
import QBit (qbitFromNumber, toColMatrix, QBitBase (..))
import Control.Monad.State (MonadState(..), StateT(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)
import Data.Complex (Complex(..))
import qualified Data.Map as Map
import Data.List (elemIndex)
import Data.Bits (testBit, setBit, clearBit, Bits (..))

-- monad transformer Reader-State-Exception that allows to go to IO in the end
newtype EvalT a = EvalT {
  runEvalT :: ReaderT Environment (StateT State (ExceptT String IO)) a
} deriving (Functor, Applicative, Monad, MonadReader Environment, MonadState State, MonadError String)

-- result of running the evaluation monad
-- we give it the initial environment and state
-- and it returns the final state and the result of the computation
run :: EvalT a -> Environment -> State -> IO (Either String (a, State))
run evalM initEnv initState =
  let evaluator = runEvalT evalM
      withInitialEnv = runReaderT evaluator initEnv
      withInitialState = runStateT withInitialEnv initState
  in
    runExceptT withInitialState

-- default environment
-- it has the standard gates defined and no circuits
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

-- default state
-- it has no qbits but allows being tensored with other qbits
defaultState :: State
defaultState = State {
  qbits = fromRowToCol $ RowMatrix 1 1 [1],
  qbitnames = [],
  qbitcontext = []
}

-- updates the state with a function
updateState :: (State -> State) -> EvalT ()
updateState f = do
  s <- get
  put $ f s

-- changes the context for the given computation
useContext :: [Name] -> EvalT a -> EvalT a
useContext names e = do
  s <- get
  put $ s { qbitcontext = names }
  result <- e
  put $ s { qbitcontext = qbitcontext s }
  return result

-- main evaluation function
eval :: QC -> EvalT ()
eval (QCCircuit _ preps body) = mapM_ eval preps >> evalSeq body
eval (QCPreparation n name) = updateState $ \s -> tensorQBit s name $ castFromInt $ toColMatrix $ qbitFromNumber n
eval _ = error "Not implemented: eval"

-- applies an operator to the state
-- NOTE: because '->' is basically flipped matrix multiplication,
-- we flip several things here to make it work.
operate :: Operator -> EvalT ()
operate op = updateState $ \s -> s { qbits = op <> qbits s } -- flipped

-- evaluates the body of a circuit and only that
-- it is used to call `local` when evaluating a gate
evalSeq :: [QC] -> EvalT ()
evalSeq [] = return ()
evalSeq ((QCOperation names qc):qcs) = evalOperator names qc >>= operate >> evalSeq qcs
evalSeq (gate@(QCGate name _ _):qcs) = local (addGate name gate) $ evalSeq qcs
evalSeq ((QCIf conditions body):qcs) = evalIf conditions body >> evalSeq qcs
evalSeq (qc:_) = error $ "Not implemented: evalSeq for " ++ show qc

-- evaluates an if statement and only that
-- fails when the variables in the conditions occur in the body
-- otherwise, it compiles the if statement to an operator and applies it
evalIf :: [QC] -> [QC] -> EvalT ()
evalIf conditions body = do
  let vs = concatMap variables conditions
  let vs' = concatMap variables body
  if vs `someOccursIn` vs'
    then throwError "Variables in if condition must not occur in body"
    else compileIf conditions body >>= operate

-- returns the variables present in an if-statement
-- NOTE: circuits and gates are not allowed inside if-statements
-- so we don't need to check for them
variables :: QC -> [Name]
variables (QCIf conditions body) = concatMap variables conditions ++ concatMap variables body
variables (QCVariable name) = [name]
variables (QCNegatedVariable name) = [name]
variables (QCOperation names _) = names
variables _ = []

-- checks for name occurrences of the first list in the second
someOccursIn :: [Name] -> [Name] -> Bool
someOccursIn [] _ = False
someOccursIn (x:xs) ys = x `elem` ys || xs `someOccursIn` ys

-- to compile an if statement, we need to move the condition qbits
-- to the front and then compile the body of the if statement
-- with a modified context (removing the qbits used as conditions)
--
-- finally we apply the operator conditionally
-- checking base-by-base if the conditions match, and use a column
-- of the operator if they do, or the identity otherwise
--
-- |cond,rest> -> (IxH)|cond,rest> when matched
-- |cond,rest> -> (IxI)|cond,rest> otherwise
--
-- where H is the compiled body, and I is the identity with
-- corresponding dimensions
compileIf :: [QC] -> [QC] -> EvalT Operator
compileIf conditions body = do
  s <- get

  let vs = concatMap variables conditions

  let context = qbitcontext s

  let conditionsBits = toBase (length context)

  let (allnames', swapop) = swapToPrefix (enumerate vs) context
  let swapinv = transpose swapop

  op <- useContext (remove vs allnames') $ compileOperator (remove vs allnames') body

  let leftover = castFromInt $ tensoreye $ length vs

  let fullop = leftover `tensor` op

  -- apply op conditionally, according to the conditions
  let op' = linearTransformation (length context) f
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

-- computes the matrix operator corresponding to a statement in our code
evalOperator :: [Name] -> QC -> EvalT Operator
evalOperator _ (QCOperation names qc) = evalOperator names qc

evalOperator names (QCArrow qc1 qc2) = do
  op1 <- evalOperator names qc1
  op2 <- evalOperator names qc2
  -- flipped
  return $ op2 <> op1

-- expand the operator according to the variables used this line
evalOperator names (QCTensors tensors) = do
  partialOps <- evalTensors names tensors
  expand names partialOps

evalOperator names (QCVariable name) = resolveVariable name >>= evalOperator names

evalOperator _ QCI = return $  castFromInt $ fromRowToCol $ RowMatrix 2 2 [1, 0, 0, 1]
evalOperator _ QCX = return $  castFromInt $ fromRowToCol $ RowMatrix 2 2 [0, 1, 1, 0]
evalOperator _ QCY = return $                fromRowToCol $ RowMatrix 2 2 [0, 0 :+ (-1), 0 :+ 1, 0]
evalOperator _ QCZ = return $  castFromInt $ fromRowToCol $ RowMatrix 2 2 [1, 0, 0, -1]
evalOperator _ QCH = return $ castFromReal $ fromRowToCol $ RowMatrix 2 2 [1 / sqrt 2, 1 / sqrt 2, 1 / sqrt 2, -1 / sqrt 2]

evalOperator _ (QCGate _ args body) = useContext args $ compileOperator args body
evalOperator names (QCIf conditions body) = useContext names $ compileIf conditions body

evalOperator _ qc = error $ "Not implemented: evalOperator for " ++ show qc

-- resolves a variable name to a gate
-- other types of variables are not implemented
resolveVariable :: Name -> EvalT QC
resolveVariable name = do
  env <- ask
  case Map.lookup name (gates env) of
    Nothing -> throwError $ "Variable " ++ name ++ " not found (in global scope, local is not implemented)"
    Just qc -> return qc

-- compiles a gate into an operator
-- by multiplying the operators corresponding to each line
compileOperator :: [Name] -> [QC] -> EvalT Operator
compileOperator args body = foldl1 (<>) <$> reversedChain
  where
    evalChain = sequenceA (evalOperator args <$> body)
    -- flipped
    reversedChain = reverse <$> evalChain

-- tensors a list of operators, and checks that the number of qbits used
-- matches the dimension of the operators being tensored
-- NOTE: we could also check by compiling the operator and checking the
-- dimension of the resulting matrix, but this fails gracefully
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

-- dimension mistmach error
mismatch :: EvalT a
mismatch = throwError "Qbit/Operator mismatch"

-- number of arguments of any gate in the language, even user-defined ones
arguments :: QC -> Int
arguments (QCGate _ names _) = length names
arguments QCI = 1
arguments QCX = 1
arguments QCY = 1
arguments QCZ = 1
arguments QCH = 1
arguments qc = error $ "Not implemented: arguments for " ++ show qc

-- a swap between the i-th and j-th qbits is a linear transformation
-- that swaps the canonical base vectors in which the bra-ket notation
-- has the i-th and j-th qbits being different from one another, and
-- having the rest be whatever value they may be
--
-- the way we do this is to test if the i-th qbit is 1 and the j-th is 0
-- or viceversa, and if so, we give back the base vector with those
-- two qbits flipped, otherwise we give back the same base vector
--
-- |..2^i..2^j..> -> |..2^j..2^i..>
-- |.. 1 .. 0 ..> -> |.. 0 .. 1 ..>
-- |.. 0 .. 1 ..> -> |.. 1 .. 0 ..>
--            |v> -> |v>
swap :: Int -> Int -> Int -> Operator
swap i j n = castFromInt $ linearTransformation n f
  where
    f base | testBit base i && not (testBit base j) = toColMatrix $ setBit (clearBit base i) j
           | not (testBit base i) && testBit base j = toColMatrix $ setBit (clearBit base j) i
           | otherwise = toColMatrix base

-- grabs all elements in the range of indices [i,j)
slice :: Int -> Int -> [a] -> [a]
slice i j xs = take (j - i) $ drop i xs

-- swaps a variable to the i-th position in a list of variables
-- and returns the new list of names, along with the swap operator
-- that acts on the system of qbits
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
      -- leftmost qbit is the most significant, this means its index is the complement
      -- of its significance, and swap works with qbit significances, so we complement
      sm = swap (n - i - 1) (n - j - 1) n
      ith = [names !! i]
      jth = [names !! j]

-- move the qbits involved in an operation to the front of the list,
-- and return the new list of names, along with the swap operator
-- that acts on the system of qbits
swapToPrefix :: [(Int,Name)] -> [Name] -> ([Name], Operator)
swapToPrefix [] allnames = (allnames, castFromInt $ tensoreye $ length allnames)
swapToPrefix ((i,name):names) allnames = (finalnames', sm' <> sm) -- flipped
  where
    (allnames', sm) = putIth i name allnames
    (finalnames', sm') = swapToPrefix names allnames'

-- expansion of a list of names and operators acting on them
-- involves tensoring the operators and adding identities for the
-- qbits that are not present in the operation
--
-- since the tensor product is not commutative, we need to swap
-- the qbits to the front of the list with the corresponding swap operator,
-- tensor the operators, tensor the leftover identities, and then
-- swap back the qbits to their original position
--
-- NOTE: this is done in part for simplicity and in part because a gate
-- can act on multiple qbits, and if they happen to not be next
-- to each other and arranged in a different order, we cannot
-- apply the gate directly as it was compiled
expand :: [Name] -> [Operator] -> EvalT Operator
expand names partialOps = do
  s <- get

  let context = qbitcontext s

  let (_, swapop) = swapToPrefix (enumerate names) context
  let swapinv = transpose swapop

  -- tensor with leftover identities
  let leftover = castFromInt $ tensoreye (length context - length names)

  let op = foldr1 tensor (partialOps ++ [leftover])

  -- swap before and after you apply the operator
  return $ swapinv <> op <> swapop
