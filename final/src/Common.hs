module Common (
  Name,
  QBit,
  Operator,
  QC(..),
  State(..),
  Environment(..),
  showState,
  Matrix(..),
  eye,
  (.>),
  (<.),
  (<+>),
  tensor,
  addCircuit,
  addGate,
  tensorQBit,
  qbitFromNumber,
) where

import Data.Complex (Complex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Name = String

data QC
  = QCCircuit Name [QC] [QC]
  | QCPreparation Int Name
  | QCGate Name [Name] [QC]
  | QCOperation [Name] QC
  | QCArrow QC QC
  | QCTensors [QC]
  | QCVariable Name
  | QCI | QCX | QCY | QCZ | QCH
  deriving Show

data Matrix a = Matrix {
  rows :: Int,
  cols :: Int,
  asList :: [a]
}

indices :: Int -> [Int]
indices n = [0..n - 1]

row :: Matrix a -> Int -> [a]
row (Matrix _ c m) i = take c $ drop (i * c) m

col :: Matrix a -> Int -> [a]
col (Matrix r c m) j = [m !! (i * c + j) | i <- indices r]

eye :: Num a => Int -> Matrix a
eye n = Matrix n n $ [if i == j then 1 else 0 | i <- indices n, j <- indices n]

instance Functor Matrix where
  fmap f (Matrix r c m) = Matrix r c $ fmap f m

checkDimensions :: Matrix a -> Matrix a -> b -> b
checkDimensions (Matrix r1 c1 _) (Matrix r2 c2 _) x =
  if r1 /= r2 || c1 /= c2
  then error "Matrix dimensions do not match"
  else x

infixl 6 <+>
(<+>) :: Num a => Matrix a -> Matrix a -> Matrix a
m1 <+> m2 = checkDimensions m1 m2 $ Matrix (rows m1) (cols m1) $ zipWith (+) (asList m1) (asList m2)

infixr 7 .>
(.>) :: Num a => a -> Matrix a -> Matrix a
x .> m = Matrix (rows m) (cols m) $ fmap (x *) (asList m)

infixl 7 <.
(<.) :: Num a => Matrix a -> a -> Matrix a
(<.) = flip (.>)

instance Num a => Semigroup (Matrix a) where
  m1 <> m2 =
    if cols m1 /= rows m2
    then error "Matrix dimensions do not match"
    else Matrix (rows m1) (cols m2) $ do
      i <- indices $ rows m1
      j <- indices $ cols m2
      return $ sum $ zipWith (*) (row m1 i) (col m2 j)

tensor :: Num a => Matrix a -> Matrix a -> Matrix a
tensor (Matrix r1 c1 m1) (Matrix r2 c2 m2) = Matrix (r1 * r2) (c1 * c2) $ do
  i <- indices r1
  j <- indices c1
  k <- indices r2
  l <- indices c2
  return $ m1 !! (i * c1 + j) * m2 !! (k * c2 + l)

type QBit = Matrix (Complex Double)
type Operator = QBit

data Environment = Environment {
  circuits :: Map Name QC,
  gates :: Map Name QC
}

-- supposed to be a column vector
data State = State {
  qbits :: QBit,
  qbitnames :: [Name],
  localenv :: Environment
}

showState :: State -> String
showState = show . asList . qbits

addCircuit :: Name -> QC -> State -> State
addCircuit name circuit state = state {
  localenv = (localenv state) {
    circuits = Map.insert name circuit (circuits $ localenv state)
  }
}

addGate :: Name -> QC -> Environment -> Environment
addGate name gate env = env {
  gates = Map.insert name gate (gates env)
}

tensorQBit :: State -> Name -> QBit -> State
tensorQBit s1 name qbit = State {
  qbits = tensor (qbits s1) qbit,
  qbitnames = qbitnames s1 ++ [name],
  localenv = localenv s1
}

qbitFromNumber :: Int -> QBit
qbitFromNumber 0 = Matrix 2 1 [1, 0]
qbitFromNumber 1 = Matrix 2 1 [0, 1]
qbitFromNumber _ = error "qbitFromNumber: number must be 0 or 1"