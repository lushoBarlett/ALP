module QBit (
  QBitBase(..),
  qbitFromNumber,
  allBases,
  toColMatrix,
  fromCols
) where

import Data.Bits
import Matrix (ColMatrix(..))

-- canonical base vectors of dimension 2^n, e_1 through e_n.
-- because in the literature, this is the result of tensoring
-- n qubits together, i.e. |1> tensor |0> tensor |1>
-- is the 8-dimensional vector |101>, we store the value as an Int
data QBitBase = QBitBase {
  baseValueRepr :: Int,
  tensorDimension :: Int
}

-- applies a function to each bit index of a base and collects the results
applyToBit :: (Int -> a) -> QBitBase -> [a]
applyToBit f b = [f i | i <- reverse [0..tensorDimension b - 1]]

instance Eq QBitBase where
  a == b = tensorDimension a == tensorDimension b && baseValueRepr a == baseValueRepr b

instance Show QBitBase where
  show b = "|" ++ applyToBit oz b ++ ">"
    where oz i = if testBit b i then '1' else '0'

-- some of these don't make sense, or are not useful, but are required
instance Bits QBitBase where
  (.&.) a b = QBitBase (baseValueRepr a .&. baseValueRepr b) (tensorDimension a)
  (.|.) a b = QBitBase (baseValueRepr a .|. baseValueRepr b) (tensorDimension a)
  xor a b = QBitBase (baseValueRepr a `xor` baseValueRepr b) (tensorDimension a)
  complement a = QBitBase (complement $ baseValueRepr a) (tensorDimension a)
  shift a i = QBitBase (shift (baseValueRepr a) i) (tensorDimension a)
  rotate a i = QBitBase (rotate (baseValueRepr a) i) (tensorDimension a)
  bitSize a = tensorDimension a
  bitSizeMaybe a = Just $ tensorDimension a
  isSigned _ = False
  testBit a i = testBit (baseValueRepr a) i
  bit i = QBitBase (bit i) (i + 1)
  popCount a = popCount $ baseValueRepr a

-- converts a number to a qbit, per our convention
qbitFromNumber :: Int -> QBitBase
qbitFromNumber 0 = QBitBase 0 1
qbitFromNumber 1 = QBitBase 1 1
qbitFromNumber _ = error "qbitFromNumber: number must be 0 or 1"

-- generates all possible canonical bases of dimension n, as QBitBases
allBases :: Int -> [QBitBase]
allBases n = [QBitBase i n | i <- [0..2^n-1]]

-- converts a qbit to a column vector
toColMatrix :: Num a => QBitBase -> ColMatrix a
toColMatrix b = ColMatrix (2^tensorDimension b) 1 $ [oz i | i <- [0..2^tensorDimension b - 1]]
  where oz i = if baseValueRepr b == i then 1 else 0

-- convers a list of column vectors to a column major matrix
fromCols :: [ColMatrix a] -> ColMatrix a
fromCols cs = ColMatrix (cmrows $ head cs) (length cs) $ concat $ cmAsList <$> cs
