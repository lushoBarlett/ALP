module QBit (
  QBitBase(..),
  allBases,
  toColMatrix,
  fromCols,
  colMatrixFromNumber
) where

import Data.Bits
import Matrix (ColMatrix(..))

-- canonical base vectors of dimension 2^n, e_1 through e_n.
-- because in the literature, this is the result of tensoring
-- n qubits together, i.e. |1> tensor |0> tensor |1>
-- is the 8-dimensional vector |101>, we store the value as an Int
data QBitBase = QBitBase {
  val :: Int,
  dim :: Int
}

-- applies a function to each bit index of a base and collects the results
applyToBit :: (Int -> a) -> QBitBase -> [a]
applyToBit f b = [f i | i <- reverse [0..dim b - 1]]

instance Eq QBitBase where
  a == b = dim a == dim b && val a == val b

instance Show QBitBase where
  show b = "|" ++ applyToBit oz b ++ ">"
    where oz i = if testBit b i then '1' else '0'

-- some of these don't make sense, or are not useful, but are required
instance Bits QBitBase where
  (.&.) a b = QBitBase (val a .&. val b) (dim a)
  (.|.) a b = QBitBase (val a .|. val b) (dim a)
  xor a b = QBitBase (val a `xor` val b) (dim a)
  complement a = QBitBase (complement $ val a) (dim a)
  shift a i = QBitBase (shift (val a) i) (dim a)
  rotate a i = QBitBase (rotate (val a) i) (dim a)
  bitSize = dim
  bitSizeMaybe = Just . dim
  isSigned _ = False
  testBit = testBit . val
  bit i = QBitBase (bit i) (i + 1)
  popCount = popCount . val

-- generates all possible canonical bases of dimension n, as QBitBases
allBases :: Int -> [QBitBase]
allBases n = [QBitBase i n | i <- [0..2^n-1]]

-- converts a qbit to a column vector
toColMatrix :: Num a => QBitBase -> ColMatrix a
toColMatrix b = ColMatrix (2^dim b) 1 $ [oz i | i <- [0..2^dim b - 1]]
  where oz i = if val b == i then 1 else 0

-- convers a list of column vectors to a column major matrix
fromCols :: [ColMatrix a] -> ColMatrix a
fromCols cs = ColMatrix (cmrows $ head cs) (length cs) $ concatMap cmAsList cs

colMatrixFromNumber :: Num a => Int -> Int -> ColMatrix a
colMatrixFromNumber n d = toColMatrix $ QBitBase n d