module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n    ) = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (LApp t u  ) = conversion' b t :@: conversion' b u
conversion' b (LAbs n t u) = Lam t (conversion' (n : b) u)
conversion' b (LLet v t u) = Let (conversion' b t) (conversion' (v : b) u)
conversion' _ LUnit        = Unit
conversion' b (LPair x y)  = Pair (conversion' b x) (conversion' b y)
conversion' b (LFst x)     = Fst (conversion' b x)
conversion' b (LSnd x)     = Snd (conversion' b x)
conversion' _ LZero        = Zero
conversion' b (LSuc x)     = Suc (conversion' b x)
conversion' b (LRec x y z) = Rec (conversion' b x) (conversion' b y) (conversion' b z)


-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let t'  u)           = Let (sub i t t') (sub (i + 1) t u)
sub _ _ Unit                  = Unit
sub i t (Pair x y)            = Pair (sub i t x) (sub i t y)
sub i t (Fst x)               = Fst (sub i t x)
sub i t (Snd x)               = Snd (sub i t x)
sub _ _ Zero                  = Zero
sub i t (Suc x)               = Suc (sub i t x)
sub i t (Rec x y z)           = Rec (sub i t x) (sub i t y) (sub i t z)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _             ) = error "variable ligada inesperada en eval"
eval e (Free  n             ) = fst $ fromJust $ lookup n e
eval _ (Lam      t   u      ) = VLam t u
eval e (Lam _ u  :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam _ u1 :@: u2)      = let v2 = eval e u2 in eval e (sub 0 (quote v2) u1)
eval e (u        :@: v      ) = case eval e u of
  VLam t u' -> eval e (Lam t u' :@: v)
  _         -> error "Esperaba una abstracción en lado izquierdo de aplicación"
eval e (Let t u)              = eval e $ sub 0 t u
eval _ Unit                   = VUnit
eval e (Pair x y)             = VPair (eval e x) (eval e y)
eval e (Fst x)                = case eval e x of
  VPair v1 _ -> v1
  _          -> error "Esperaba un par en aplicación de fst"
eval e (Snd x) = case eval e x of
  VPair _ v2 -> v2
  _          -> error "Esperaba un par en aplicación de snd"
eval _ Zero     = VNum NZero
eval e (Suc x)  = case eval e x of
  VNum n -> VNum (NSuc n)
  _      -> error "Esperaba un número natural en aplicación de suc"
eval e (Rec b r Zero)    = eval e b
eval e (Rec b r (Suc n)) = eval e (r :@: Rec b r n :@: n)


-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f) = Lam t f
quote VUnit      = Unit
quote (VPair x y) = Pair (quote x) (quote y)
quote (VNum n)    = quoteNat n

quoteNat :: NumVal -> Term
quoteNat NZero     = Zero
quoteNat (NSuc n') = Suc (quoteNat n')

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i)  = ret (c !! i)
infer' _ e (Free  n)  = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u)  = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u)  = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let t u)  = infer' c e t >>= \tt -> infer' (tt : c) e u
infer' _ _ Unit       = ret UnitT
infer' c e (Pair x y) =
  infer' c e x >>= \tx -> infer' c e y >>= \ty -> ret $ PairT tx ty
infer' c e (Fst x) = infer' c e x >>= \tx -> case tx of
  PairT t1 _ -> ret t1
  _          -> err $ "Se esperaba un par"
infer' c e (Snd x) = infer' c e x >>= \tx -> case tx of
  PairT _ t2 -> ret t2
  _          -> err $ "Se esperaba un par"
infer' _ _ Zero = ret NatT
infer' c e (Suc x) = infer' c e x >>= \tx -> case tx of
  NatT -> ret NatT
  _    -> err $ "Se esperaba un número natural"
infer' c e (Rec b r n) = infer' c e b >>= \tb -> infer' c e r >>= \tr -> infer' c e n >>= \tn -> analize tb tr tn
  where
    analize :: Type -> Type -> Type -> Either String Type
    analize tb tr tn | tn /= NatT = err $ "Se esperaba un número natural"
                     | otherwise  = case tr of
                                    (FunT t (FunT n t')) -> if (t == tb && t == t' && n == NatT) then ret t else err $ "Se esperaba una función de tipo " ++ render (printType tb) ++ " -> " ++ render (printType NatT) ++ " -> " ++ render (printType tb)
                                    _ -> err $ "Se esperaba una función de tipo " ++ render (printType tb) ++ " -> " ++ render (printType NatT) ++ " -> " ++ render (printType tb)
----------------------------------
