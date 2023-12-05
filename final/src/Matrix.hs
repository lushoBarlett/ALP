{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Matrix (
  Matrix(..),
  RowMatrix(..),
  ColMatrix(..),
  fromRowToCol,
  fromColToRow
) where

dot :: Num a => [a] -> [a] -> a
dot as bs = sum $ zipWith (*) as bs

class (Eq a, Show a, Semigroup m) => Matrix m a | m -> a where
  rows :: m -> Int

  cols :: m -> Int

  (!) :: m -> (Int, Int) -> a

  transpose :: m -> m

  tensor :: m -> m -> m

  row :: Int -> m -> [a]
  row i m = [m ! (i, j) | j <- [0..cols m - 1]]

  col :: Int -> m -> [a]
  col j m = [m ! (i, j) | i <- [0..rows m - 1]]

  _eq :: m -> m -> Bool
  _eq m1 m2 = rows m1 == rows m2 && cols m1 == cols m2 && and [m1 ! (i, j) == m2 ! (i, j) | i <- [0..rows m1 - 1], j <- [0..cols m1 - 1]]

  _show :: m -> String
  _show m = unlines [unwords [show $ m ! (i, j) | j <- [0..cols m - 1]] | i <- [0..rows m - 1]]

data RowMatrix a = RowMatrix {
  rmrows :: Int,
  rmcols :: Int,
  rmAsList :: [a]
}

instance (Num a, Show a, Eq a) => Semigroup (RowMatrix a) where
  m1 <> m2 = if cols m1 /= rows m2
    then error $ "incompatible dimensions " ++ show (cols m1) ++ " " ++ show (rows m2)
    else RowMatrix (rows m1) (cols m2) $ do
      -- traverse by rows, because we build in rows
      i <- [0..rows m1 - 1]
      j <- [0..cols m2 - 1]
      return $ row i m1 `dot` col j m2

instance (Num a, Show a, Eq a) => Matrix (RowMatrix a) a where
  rows = rmrows
  cols = rmcols
  (RowMatrix _ c as) ! (i, j) = as !! (i * c + j)
  transpose m@(RowMatrix r c _) = RowMatrix c r $ do
    j <- [0..c-1]
    i <- [0..r-1]
    return $ m ! (i,j)
  tensor m1@(RowMatrix r1 c1 _) m2@(RowMatrix r2 c2 _) = RowMatrix (r1 * r2) (c1 * c2) $ do
    --rows
    i1 <- [0..r1-1]
    i2 <- [0..r2-1]
    --cols
    j1 <- [0..c1-1]
    j2 <- [0..c2-1]
    return $ m1 ! (i1,j1) * m2 ! (i2,j2)

data ColMatrix a = ColMatrix {
  cmrows :: Int,
  cmcols :: Int,
  cmAsList :: [a]
}

instance (Num a, Show a, Eq a) => Semigroup (ColMatrix a) where
  m1 <> m2 = if cols m1 /= rows m2
    then error $ "incompatible dimensions " ++ show (cols m1) ++ " " ++ show (rows m2)
    else ColMatrix (rows m1) (cols m2) $ do
      -- traverse by columns, because we build in columns
      j <- [0..cols m2 - 1]
      i <- [0..rows m1 - 1]
      return $ row i m1 `dot` col j m2

instance (Num a, Show a, Eq a) => Matrix (ColMatrix a) a where
  rows = cmrows
  cols = cmcols
  (ColMatrix _ c as) ! (i, j) = as !! (j * c + i)
  transpose m@(ColMatrix r c _) = ColMatrix c r $ do
    j <- [0..c-1]
    i <- [0..r-1]
    return $ m ! (i,j)
  tensor m1@(ColMatrix r1 c1 _) m2@(ColMatrix r2 c2 _) = ColMatrix (r1 * r2) (c1 * c2) $ do
    --cols
    j1 <- [0..c1-1]
    j2 <- [0..c2-1]
    --rows
    i1 <- [0..r1-1]
    i2 <- [0..r2-1]
    return $ m1 ! (i1,j1) * m2 ! (i2,j2)

instance (Num a, Show a, Eq a) => Show (RowMatrix a) where
  show = _show

instance (Num a, Show a, Eq a) => Show (ColMatrix a) where
  show = _show

instance (Num a, Show a, Eq a) => Eq (RowMatrix a) where
  (==) = _eq

instance (Num a, Show a, Eq a) => Eq (ColMatrix a) where
  (==) = _eq

instance Functor RowMatrix where
  fmap f (RowMatrix r c as) = RowMatrix r c $ fmap f as

instance Functor ColMatrix where
  fmap f (ColMatrix r c as) = ColMatrix r c $ fmap f as

fromRowToCol :: (Num a, Show a, Eq a) => RowMatrix a -> ColMatrix a
fromRowToCol m@(RowMatrix r c _) = ColMatrix r c $ do
  j <- [0..c-1]
  i <- [0..r-1]
  return $ m ! (i,j)

fromColToRow :: (Num a, Show a, Eq a) => ColMatrix a -> RowMatrix a
fromColToRow m@(ColMatrix r c _) = RowMatrix r c $ do
  i <- [0..r-1]
  j <- [0..c-1]
  return $ m ! (i,j)
