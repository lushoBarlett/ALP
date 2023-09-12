module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "repeat", "skip", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

-- Santa's little helper function
intBinOp opstring op = do
  reservedOp lis opstring
  e2 <- intexp
  return $ \e1 -> op e1 e2

boolBinOp opstring op = do
  reservedOp lis opstring
  e2 <- boolexp
  return $ \e1 -> op e1 e2

----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = do
  e <- atom
  case e of
    Var v -> (do f <- assgn; return $ f v) <|> (applyBins e) <|> (return e)
    _ -> (applyBins e) <|> (return e)
  where
    applyBins e = do f <- bins; return $ f e
    atom = pint <|> pvar <|> parenthesis <|> opposite
    bins = plus <|> minus <|> times <|> div <|> seq

    pint = Const . fromInteger <$> integer lis
    pvar = Var <$> identifier lis
    parenthesis = parens lis intexp

    opposite = reservedOp lis "-" >> UMinus <$> intexp

    plus = intBinOp "+" Plus
    minus = intBinOp "-" Minus
    times = intBinOp "*" Times
    div = intBinOp "/" Div

    assgn = do
      reservedOp lis "="
      e <- intexp
      return $ \v -> EAssgn v e

    seq = intBinOp "," ESeq

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp =
  (do e <- atom
      (do f <- boolBins; return $ f e) <|> return e)
  <|>
  (do i <- intexp
      f <- intBins
      return $ f i)
  where
    atom = ptrue <|> pfalse <|> parenthesis <|> negation
    intBins = lt <|> gt <|> eq <|> neq
    boolBins = and <|> or

    ptrue  = reserved lis "true" >> return BTrue
    pfalse = reserved lis "false" >> return BFalse
    negation = reservedOp lis "!" >> Not <$> boolexp
    parenthesis = parens lis boolexp

    lt = intBinOp "<" Lt
    gt = intBinOp ">" Gt
    eq = intBinOp "==" Eq
    neq = intBinOp "!=" NEq

    and = boolBinOp "&&" And
    or = boolBinOp "||" Or

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = do
  l <- line
  (do f <- pseq; return $ f l) <|> return l
  where
    line = pskip <|> passign <|> pifelse <|> prepeat

    pskip = reserved lis "skip" >> return Skip

    passign = do
      v <- identifier lis
      reservedOp lis "="
      e <- intexp
      return $ Let v e

    pifelse = do
      reserved lis "if"
      b <- boolexp
      c <- braces lis comm
      (do f <- pelse; return $ f b c) <|> (return $ IfThen b c)

    pelse = do
      reserved lis "else"
      c2 <- braces lis comm
      return $ \b -> (\c1 -> IfThenElse b c1 c2)

    prepeat = do
      reserved lis "repeat"
      c <- braces lis comm
      reserved lis "until"
      b <- boolexp
      return $ Repeat c b

    pseq = do
      reservedOp lis ";"
      c <- comm
      return $ \c1 -> Seq c1 c

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
