module Common (
  Name,
  Operator,
  QC(..),
  State(..),
  Environment(..),
  linearTransformation,
  tensoreye,
  addCircuit,
  addGate,
  showState,
  tensorQBit,
  castFromInt,
  castFromReal
) where

import Data.Complex (Complex(..))
import qualified Data.Map.Strict as Map
import Matrix (Matrix(..), ColMatrix(..))
import QBit (QBitBase(..), allBases, toColMatrix, fromCols)

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

linearTransformation :: Int -> (QBitBase -> QBitBase) -> ColMatrix Int
linearTransformation n f = fromCols $ toColMatrix . f <$> allBases n

tensoreye :: Int -> ColMatrix Int
tensoreye n = linearTransformation n id

type QBit = ColMatrix (Complex Double)
type Operator = QBit

data Environment = Environment {
  circuits :: Map.Map Name QC,
  gates :: Map.Map Name QC
}

-- supposed to be a column vector
data State = State {
  qbits :: QBit,
  qbitnames :: [Name]
}

showState :: State -> String
showState state = concat $ ppcomplex <$> cmAsList (qbits state)
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

castFromInt :: (RealFloat a, Functor f) => f Int -> f (Complex a)
castFromInt = fmap (:+ 0) . fmap fromIntegral

castFromReal :: (RealFloat a, Functor f) => f a -> f (Complex a)
castFromReal = fmap (:+ 0)
