module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion lterm = c' lterm []
  where
    c' :: LamTerm -> [String] -> Term
    c' (LVar v)    nameEnv = case elemIndex v nameEnv of
      Just i  -> Bound i
      Nothing -> Free (Global v)
    c' (App t1 t2) nameEnv = c' t1 nameEnv :@: c' t2 nameEnv
    c' (Abs v t)   nameEnv = Lam $ c' t (v:nameEnv)
-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f) v = f v
vapp (VNeutral neu) v = VNeutral $ NApp neu v

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' (Free n) (gEnv, _) = case lookup n gEnv of
  Just v  -> v
  Nothing -> VNeutral $ NFree n
eval' (t1 :@: t2) env = vapp (eval' t1 env) (eval' t2 env)
eval' (Lam t) (gEnv, lEnv) = VLam $ \v -> eval' t (gEnv, v:lEnv)


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

replace :: Int -> Term -> Term
replace total (Free (Quote k)) = Bound (total - k - 1)
replace total (Free n) = Free n
replace total (t1 :@: t2) = replace total t1 :@: replace total t2
replace total (Lam t) = Lam $ replace total t
replace total (Bound ii) = Bound ii

quote :: Value -> Term
quote v =
  let (t, total) = quote' 0 v
   in t

quote' :: Int -> Value -> (Term, Int)
quote' k (VLam f) =
  let (fterm, total) = quote' (k + 1) $ f value
   in (replace total $ Lam fterm, total)
  where
    value = VNeutral $ NFree (Quote k)
quote' k (VNeutral neu) = quoteNeutral k neu

quoteNeutral :: Int -> Neutral -> (Term, Int)
quoteNeutral k (NFree n) = (Free n, k)
quoteNeutral k (NApp neu v) =
  let (t, total) = quoteNeutral k neu
      (u, total') = quote' total v
   in (t :@: u, total')
