module Practica1 where

import Data.Char ( digitToInt )
import Control.Applicative ( Alternative((<|>), empty) )
import Parsing ( Parser, parse, digit, char, token, sepBy, string, item, int, identifier )

-- Ejercicio 1
-- sepBy parsea 0 o más ocurrencias de p, separadas por exáctamente una ocurrencia de sep.
-- symbol parsea un string dado, posiblmente con espacios antes y después.

-- Ejercicio 2
expr :: Parser Int
expr = do
  t <- term
  (do
    char '+'
    e <- expr
    return (t + e))
    <|>
    (do
      char '-'
      e <- expr
      return (t - e))
      <|> return t

term :: Parser Int
term = do
  f <- factor
  (do
    char '*'
    t <- term
    return (f * t))
    <|>
    (do
      char '/'
      t <- term
      return (f `div` t))
      <|> return f

factor :: Parser Int
factor = do
  d <- digit
  return (digitToInt d)
  <|> do
    char '('
    e <- expr
    char ')'
    return e

eval :: String -> Int
eval xs = fst (head (parse expr xs))

-- Ejericio 3
withParentheses :: Parser a -> Parser a
withParentheses p = do
  char '('
  x <- p
  char ')'
  return x

-- Ejercicio 4
data Expr = Num Int | BinOp Op Expr Expr deriving (Show)
data Op = Add | Sub | Mul | Div deriving (Show)

expr' :: Parser Expr
expr' = do
  t <- term'
  (do
    char '+'
    e <- expr'
    return (BinOp Add t e))
    <|>
    (do
      char '-'
      e <- expr'
      return (BinOp Sub t e))
      <|> return t

term' :: Parser Expr
term' = do
  f <- factor'
  (do
    char '*'
    t <- term'
    return (BinOp Mul f t))
    <|>
    (do
      char '/'
      t <- term'
      return (BinOp Div f t))
      <|> return f

factor' :: Parser Expr
factor' = do
  d <- digit
  return (Num $ digitToInt d)
  <|> do
    char '('
    e <- expr'
    char ')'
    return e

-- Ejercicio 5

{-

La gramatica esta dada por

Hasktype => Base | Fun
Fun => Base -> Base | Base -> Fun
Base => Int | Char | Float

-}

data Basetype = DInt | DChar | DFLoat deriving (Show)
type Hasktype = [Basetype]

hasktype :: Parser Hasktype
hasktype = sepBy types arrow
  where
    arrow = token $ string "->"
    types = dint <|> dchar <|> dfloat
    dint = DInt <$ string "Int"
    dchar = DChar <$ string "Char"
    dfloat = DFLoat <$ string "Float"

-- Ejericio 6

{-

La gramatica esta dada por

S -> T | T -> S
T -> Int | Char | Float

-}

data HetItem = HInt Int | HChar Char deriving (Show)
type HetList = [HetItem]

hetlist :: Parser HetList
hetlist = char '[' *> sepBy items arrow <* char ']'
  where
    items = hint <|> hchar
    -- helpers
    arrow = token $ string ","
    hint = HInt <$> int
    hchar = char '\'' *> (HChar <$> item) <* char '\''

-- Ejercicio 7

-- reutilizamos la gramatica del ejercicio 5, y la hacemos en serio

data Hasktype' = DInt' | DChar' | DFLoat' | Fun' Hasktype' Hasktype' deriving (Show)
hasktype' :: Parser Hasktype'
hasktype' = base <|> fun
  where
    -- was optimized to avoid backtracking. ! it fails in the last case anyway
    fun = base >> arrow >> (fun <|> base)
    base = int <|> char <|> float
    -- helpers
    arrow = token $ string "->"
    int = DInt' <$ string "Int"
    char = DChar' <$ string "Char"
    float = DFLoat' <$ string "Float"

-- Ejercicio 8

{-

expr => expr (+ term | - term) | term
term => term (* factor | / factor ) | factor
factor => digit | ( expr )
digit => 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

=== <por eliminacion de la recursividad a izquierda>

expr => term expr'
expr' => e | (+ term expr' | - term expr')

term => factor term'
term' => e | (* factor term' | / factor term')

factor => digit | ( expr )
digit => 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

-}

exprMain :: Parser Int
exprMain = do
  t <- termMain
  f <- exprSub
  return $ f t

exprSub :: Parser (Int -> Int)
exprSub = do
  char '+'
  t <- termMain
  f <- exprSub
  return (f . (+) t)
  <|>
  do
    char '-'
    t <- termMain
    f <- exprSub
    return (f . (-) t)
    <|> return id

termMain :: Parser Int
termMain = do
  f <- factorMain
  h <- termSub
  return $ h f

termSub :: Parser (Int -> Int)
termSub = do
  char '*'
  f <- factorMain
  g <- termSub
  return (g . (*) f)
  <|>
  do
    char '/'
    f <- factorMain
    g <- termSub
    return (g . div f)
    <|> return id

factorMain :: Parser Int
factorMain = do
  d <- digit
  return (digitToInt d)
  <|> do
    char '('
    e <- exprMain
    char ')'
    return e

-- Ejericio 9

{-

declaration => typespecifier declarator ;
declarator => * declarator | directdeclarator
directdeclarator => directdeclarator [ constantexpression ] | ( directdeclarator ) | identifier
typespecifier => ’int’ | ’char’ | ’float’
constantexpression => number

Esta gramática tiene recursión por izquierda en directdeclarator, por lo que la reescribimos como

directdeclarator => identifier directdeclarator' | ( directdeclarator ) directdeclarator'
directdeclarator' => e | [ constantexpression ] directdeclarator'

-}

data CTypespec = CInt | CChar | CFloat deriving (Show)

data CDirectDeclarator
  = CIdentifier String
  | CParen CDirectDeclarator
  | CArray CDirectDeclarator Int
  deriving (Show)

data CDeclarator
  = CPointer CDeclarator
  | CDeclarator CDirectDeclarator
  deriving (Show)

data CType
  = CDecl CTypespec CDeclarator
  deriving (Show)

ctype :: Parser CType
ctype = do
  t <- typespecifier
  d <- declarator
  char ';'
  return $ CDecl t d
  where
    declarator = cpointer <|> CDeclarator <$> directdeclarator
    cpointer = CPointer <$> (char '*' *> declarator)

    directdeclarator = do
      i <- ident
      d <- directdeclarator'
      return $ d i
      <|>
      do
        d <- char '(' *> directdeclarator <* char ')'
        d' <- directdeclarator'
        return $ d' d

    -- No entiendo si esto debería ser una función o qué onda
    directdeclarator' :: Parser CDirectDeclarator
    directdeclarator' = array <|> return id

    ident = CIdentifier <$> identifier
    paren = CParen <$> (char '(' *> directdeclarator <* char ')')

    array = do
      constant <- char '[' *> int <* char ']'
      d <- directdeclarator'
      return $ CArray d constant

    typespecifier = cint <|> cchar <|> cfloat

    cint = CInt <$ string "int"
    cchar = CChar <$ string "char"
    cfloat = CFloat <$ string "float"