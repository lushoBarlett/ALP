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
replace i (Free (Quote k)) = Bound $ i - k - 1
replace i (Free n) = Free n
replace i (t1 :@: t2) = replace i t1 :@: replace i t2
replace i (Lam t) = Lam $ replace (i + 1) t
replace i (Bound ii) = Bound ii -- ???

quote :: Value -> Term
quote v = quote' 0 v

quote' :: Int -> Value -> Term
quote' k (VLam f) =
  let fterm = quote' (k + 1) $ f value
   in replace k $ Lam fterm
  where
    value = VNeutral $ NFree (Quote k)
quote' k (VNeutral neu) = quoteNeutral k neu

quoteNeutral :: Int -> Neutral -> Term
quoteNeutral k (NFree n) = Free n
quoteNeutral k (NApp neu v) =
  let t = quoteNeutral k neu
      u = quote' k v
   in t :@: u
