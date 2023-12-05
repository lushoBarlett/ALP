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
  castFromReal,
  enumerate,
  renumerate
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
  | QCIf [QC] [QC]
  | QCArrow QC QC
  | QCTensors [QC]
  | QCVariable Name
  | QCNegatedVariable Name
  | QCI | QCX | QCY | QCZ | QCH
  deriving Show

linearTransformation :: Int -> (QBitBase -> ColMatrix a) -> ColMatrix a
linearTransformation n f = fromCols $ f <$> allBases n

tensoreye :: Int -> ColMatrix Int
tensoreye n = linearTransformation n toColMatrix

type QBit = ColMatrix (Complex Double)
type Operator = QBit

data Environment = Environment {
  circuits :: Map.Map Name QC,
  gates :: Map.Map Name QC
}

-- supposed to be a column vector
data State = State {
  qbits :: QBit,
  qbitnames :: [Name],
  qbitcontext :: [Name]
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
  qbitnames = qbitnames s1 ++ [name],
  qbitcontext = qbitcontext s1 ++ [name]
}

castFromInt :: (RealFloat a, Functor f) => f Int -> f (Complex a)
castFromInt = fmap $ (:+ 0) . fromIntegral

castFromReal :: (RealFloat a, Functor f) => f a -> f (Complex a)
castFromReal = fmap (:+ 0)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

renumerate :: Int -> [a] -> [(Int, a)]
renumerate n = zip $ reverse [0..n-1]
