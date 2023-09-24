{-# OPTIONS_GHC  -Wno-overlapping-patterns #-}

module PPLis where

import           AST
import           Text.PrettyPrint
import           Prelude                 hiding ( (<>) )

tabW :: Int
tabW = 2

pVar :: Variable -> Doc
pVar = text

pExp :: Exp a -> Doc
pExp (Const  i )  = text "(" <> int i <> text ")"
pExp (Var    x )  = text "(" <> pVar x <> text ")"
pExp (UMinus n )  = text "(" <> text "-" <+> pExp n <> text ")"
pExp (Plus  a b)  = text "(" <> pExp a <+> text "+" <+> pExp b <> text ")"
pExp (Times a b)  = text "(" <> pExp a <+> text "*" <+> pExp b <> text ")"
pExp (Minus a b)  = text "(" <> pExp a <+> text "-" <+> pExp b <> text ")"
pExp (Div   a b)  = text "(" <> pExp a <+> text "/" <+> pExp b <> text ")"
pExp BTrue        = text "(" <> text "true" <> text ")"
pExp BFalse       = text "(" <> text "false" <> text ")"
pExp (Eq  a b)    = text "(" <> pExp a <+> text "==" <+> pExp b <> text ")"
pExp (NEq a b)    = text "(" <> pExp a <+> text "!=" <+> pExp b <> text ")"
pExp (Lt  a b)    = text "(" <> pExp a <+> text "<" <+> pExp b <> text ")"
pExp (Gt  a b)    = text "(" <> pExp a <+> text ">" <+> pExp b <> text ")"
pExp (And a b)    = text "(" <> pExp a <+> text "&&" <+> pExp b <> text ")"
pExp (Or  a b)    = text "(" <> pExp a <+> text "||" <+> pExp b <> text ")"
pExp (Not b  )    = text "(" <> text "!" <+> pExp b <> text ")"
pExp (EAssgn x e) = text "(" <> pVar x <+> text "=" <+> pExp e <> text ")"
pExp (ESeq a b)   = text "(" <> pExp a <> text "," <+> pExp b <> text ")"
pExp _ =
  error
    "Tipo de expresión no reconocido por el Pretty Printer."

pComm :: Comm -> Doc
pComm Skip        = text "skip"
pComm (Let x  e ) = pVar x <+> text "=" <+> pExp e
pComm (Seq c1 c2) = pComm c1 <> semi $$ pComm c2
pComm (IfThen b c) =
  text "if" <+> parens (pExp b) <+> lbrace $$ nest tabW (pComm c) $$ rbrace
pComm (IfThenElse b c1 c2) =
  text "if"
    <+> parens (pExp b)
    <+> lbrace
    $$  nest tabW (pComm c1)
    $$  rbrace
    <+> text "else"
    <+> lbrace
    $$  nest tabW (pComm c2)
    $$  rbrace
pComm (Repeat c b) =
  text "repeat" <+> lbrace $$ nest tabW (pComm c) $$ rbrace <+> text "until"  <+> parens (pExp b)

renderComm :: Comm -> String
renderComm = render . pComm

renderExp :: Exp a -> String
renderExp = render . pExp

