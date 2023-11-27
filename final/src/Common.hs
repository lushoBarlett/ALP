module Common (
  Name,
  QBit,
  Operator,
  QC(..),
  State(..),
  Environment(..),
  showState,
  eye,
  tensoreye,
  addCircuit,
  addGate,
  tensorQBit,
  qbitFromNumber,
  qbitStateFromBase,
  allBases
) where

import Data.Complex (Complex(..))
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

indices :: Int -> [Int]
indices n = [0..n - 1]

eye :: Num a => Int -> RowMatrix a
eye n = Matrix n n $ [if i == j then 1 else 0 | i <- indices n, j <- indices n]

tensoreye :: Num a => Int -> RowMatrix a
tensoreye n = eye $ 2 ^ n

type QBit = Matrix (Complex Double)
type Operator = QBit

data Environment = Environment {
  circuits :: Map Name QC,
  gates :: Map Name QC
}

-- supposed to be a column vector
data State = State {
  qbits :: QBit,
  qbitnames :: [Name]
}

showState :: State -> String
showState state = concat $ ppcomplex <$> asList (qbits state)
  where ppcomplex (r :+ i) = concat [show r, " + ", show i, "i\n"]

addCircuit :: Name -> QC -> Environment -> Environment
addCircuit name circuit env = env {
  circuits = Map.insert name circuit (circuits env)
}

addGate :: Name -> QC -> Environment -> Environment
addGate name gate env = env {
  gates = Map.insert name gate (gates env)
}

tensorQBit :: State -> Name -> QBit -> State
tensorQBit s1 name qbit = State {
  qbits = tensor (qbits s1) qbit,
  qbitnames = qbitnames s1 ++ [name]
}

fromColumns :: Num a => [[a]] -> Matrix a
fromColumns columns = Matrix n n $ do
  i <- indices n
  j <- indices n
  return $ columns !! j !! i
  where n = length columns

toColumn :: QBit -> [Complex Double]
toColumn qbit = asList $ transpose qbit

fromLinearTransformation f n = fromColumns $ toColumn . qbitStateFromBase . f <$> allBases n
