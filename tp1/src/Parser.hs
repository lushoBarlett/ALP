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

----------------------------------
--- Parser de expressiones enteras
-----------------------------------

{-
 - try en assgn es utilizado por el caso donde
 - el parser confunda una variable en una expresión cualquiera
 - con una asignación. Queremos que no consuma la variable.
 -
 - Es importante notar que no puede haber otra asignación luego de que
 - ya estemos parseando una, pero tampoco queremos parsear otra ',',
 - porque esto se saltearía las reglas de precedencia.
 - Por lo tanto lo que sigue es un parser `expr`.
 -}
intexp :: Parser (Exp Int)
intexp = seq
  where
    seq = do a <- try assgn <|> expr
             (reservedOp lis "," >> ESeq a <$> seq)
              <|> return a
    assgn = do a@(Var v) <- pvar
               reservedOp lis "=" >> EAssgn v <$> expr
    expr = do a <- factor
              (reservedOp lis "+" >> Plus a <$> expr)
               <|> (reservedOp lis "-" >> Minus a <$> expr)
               <|> return a
    factor = do a <- atom
                (reservedOp lis "*" >> Times a <$> factor)
                 <|> (reservedOp lis "/" >> Div a <$> factor)
                 <|> return a
    atom = pint <|> pvar <|> parenthesis <|> opposite
    pint = Const . fromInteger <$> integer lis
    pvar = Var <$> identifier lis
    parenthesis = parens lis intexp
    opposite = reservedOp lis "-" >> UMinus <$> intexp


-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

{-
 - try en parentesis es utilizado porque los parentesis pueden pertenecer
 - a diferentes expresiones, o sea, los parentesis estan tipados.
 - Por ejemplo:
 - (x + 1) < (x + 2) vs (x + 1 < x + 2)
 - Esto significa que los paréntesis que nos encontremos
 - pueden corresponder en realidad a una expresión entera.
 -}
boolexp :: Parser (Exp Bool)
boolexp = or
  where
    or = do  a <- and
             (reservedOp lis "||" >> Or a <$> or)
              <|> return a
    and = do a <- atom <|> not <|> comparison
             (reservedOp lis "&&" >> And a <$> and)
              <|> return a
    atom = ptrue <|> pfalse <|> try parenthesis
    not = reservedOp lis "!" >> Not <$> atom
    comparison = do a <- intexp
                    (reservedOp lis "==" >> Eq a <$> intexp)
                     <|> (reservedOp lis "!=" >> NEq a <$> intexp)
                     <|> (reservedOp lis "<" >> Lt a <$> intexp)
                     <|> (reservedOp lis ">" >> Gt a <$> intexp)
    ptrue  = reserved lis "true" >> return BTrue
    pfalse = reserved lis "false" >> return BFalse
    parenthesis = parens lis boolexp

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
      Let v <$> intexp

    pifelse = do
      reserved lis "if"
      b <- boolexp
      c <- braces lis comm
      (do f <- pelse; return $ f b c) <|> return (IfThen b c)

    pelse = do
      reserved lis "else"
      c2 <- braces lis comm
      return $ \b c1 -> IfThenElse b c1 c2

    prepeat = do
      reserved lis "repeat"
      c <- braces lis comm
      reserved lis "until"
      Repeat c <$> boolexp

    pseq = do
      reservedOp lis ";"
      c <- comm
      return $ \c1 -> Seq c1 c

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
