module PrettyPrinter
  ( printTerm
  ,     -- pretty printer para terminos
    printType     -- pretty printer para tipos
  )
where

import           Common
import           Text.PrettyPrint.HughesPJ
import           Prelude                 hiding ( (<>) )
-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s
pp _  _  Unit               = text "unit"
pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
pp ii vs (Let t u) =
  text "let"
    <+> text (vs !! ii)
    <+> text "="
    <+> pp ii vs t
    <+> text "in"
    <+> pp (ii + 1) vs u
pp ii vs (Pair t u) =
  text "("
    <> pp ii vs t
    <> text ", "
    <> pp ii vs u
    <> text ")"
pp ii vs (Fst t)  = text "fst" <+> parensIf (isCompound t) (pp ii vs t)
pp ii vs (Snd t)  = text "snd" <+> parensIf (isCompound t) (pp ii vs t)
pp ii vs Zero     = text "0"
pp ii vs (Suc t)  = case toInt t of
  Just n  -> text $ show (n + 1)
  Nothing -> text "suc" <+> parensIf (isCompound t) (pp ii vs t)
pp ii vs (Rec t1 t2 t3) =
  text "rec"
    <+> parensIf (isCompound t1) (pp ii vs t1)
    <+> parensIf (isCompound t1) (pp ii vs t2)
    <+> parensIf (isCompound t1) (pp ii vs t3)

toInt :: Term -> Maybe Int
toInt Zero    = return 0
toInt (Suc t) = (+ 1) <$> toInt t
toInt _       = Nothing

isCompound :: Term -> Bool
isCompound (Lam _ _)   = True
isCompound (Let _ _)   = True
isCompound (Fst _)     = True
isCompound (Snd _)     = True
isCompound (i :@: c)   = True
isCompound (Rec _ _ _) = True
isCompound _           = False

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _         = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
printType UnitT = text "Unit"
printType (PairT t1 t2) = text "( " <> parensIf (isFun t1) (printType t1) <> text "," <+> printType t2 <> text ")"
printType NatT = text "Nat"


isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _         ) = []
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
fv (Let t u         ) = fv t ++ fv u
fv Unit               = []
fv (Pair t u)         = fv t ++ fv u
fv (Fst t)            = fv t
fv (Snd t)            = fv t
fv Zero               = []
fv (Suc t)            = fv t
fv (Rec t1 t2 t3)     = fv t1 ++ fv t2 ++ fv t3

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

