{
module Parser where
import AST(QC(..), AngleExpr(..))
import Data.Char
}

%name qcparser
%tokentype { Token }
%error { parseError }
%monad { E } { thenE } { returnE }

%token
  '('  { TokenLParen }
  ')'  { TokenRParen }
  ';'  { TokenSemicolon }
  '|'  { TokenPipe }
  '{'  { TokenLBrace }
  '}'  { TokenRBrace }
  SKIP { TokenSkip }
  RX   { TokenRx }
  RY   { TokenRy }
  RZ   { TokenRz }
  SWAP { TokenSwap }
  CTRL { TokenControl }
  '+'  { TokenPlus }
  '-'  { TokenMinus }
  '*'  { TokenProd }
  '/'  { TokenDiv }
  PI   { TokenPi }
  FLit { TokenNumber $$ }

%left ';'
%left '|'
%left "+" "-"
%left "*" "/"

%%

Q :: { QC }
Q : '(' Q ')' { $2 }
  | SKIP { QCSkip }
  | RX '(' AngleExpr ')' { QCRx $3 }
  | RY '(' AngleExpr ')' { QCRy $3 }
  | RZ '(' AngleExpr ')' { QCRz $3 }
  | SWAP { QCSwap }
  | CTRL '{' Q '}' { QCControl $3 }
  | Q ';' Q { QCSeq $1 $3 }
  | Q '|' Q { QCPar $1 $3 }

AngleExpr : PI { AnglePi }
          | FLit { AngleConst $1 }
          | '-' AngleExpr { AngleNeg $2 }
          | AngleExpr '+' AngleExpr { $1 `AngleAdd` $3 }
          | AngleExpr '-' AngleExpr { $1 `AngleSub` $3 }
          | AngleExpr '*' AngleExpr { $1 `AngleMul` $3 }
          | AngleExpr '/' AngleExpr { $1 `AngleDiv` $3 }
          | '(' AngleExpr ')' { AngleParen $2 }
{
data Token
  = TokenLParen
  | TokenRParen
  | TokenSemicolon
  | TokenPipe
  | TokenLBrace
  | TokenRBrace
  | TokenSkip
  | TokenRx
  | TokenRy
  | TokenRz
  | TokenSwap
  | TokenControl
  | TokenPlus
  | TokenMinus
  | TokenProd
  | TokenDiv
  | TokenPi
  | TokenNumber Double
  deriving Show

parseError :: [Token] -> E a
parseError tokenList = failE ("Parse error on token list: " ++ show tokenList)

-- stolen from the Happy documentation
-- https://haskell-happy.readthedocs.io/en/latest/using.html#monadic-parsers
data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
   case m of
       Ok a     -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
   case m of
      Ok a     -> Ok a
      Failed e -> k e

lexer :: String -> [Token]
lexer [] = []
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer (';':cs) = TokenSemicolon : lexer cs
lexer ('|':cs) = TokenPipe : lexer cs
lexer ('{':cs) = TokenLBrace : lexer cs
lexer ('}':cs) = TokenRBrace : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenProd : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('π':cs) = TokenPi : lexer cs
lexer (c:cs) | isSpace c = lexer cs
lexer (c:cs) | isDigit c = lexNum (c:cs)
lexer cs
  | "skip" `isPrefixOf` cs = TokenSkip : lexer (drop 4 cs)
  | "Rx" `isPrefixOf` cs = TokenRx : lexer (drop 2 cs)
  | "Ry" `isPrefixOf` cs = TokenRy : lexer (drop 2 cs)
  | "Rz" `isPrefixOf` cs = TokenRz : lexer (drop 2 cs)
  | "swap" `isPrefixOf` cs = TokenSwap : lexer (drop 4 cs)
  | "ctrl" `isPrefixOf` cs = TokenControl : lexer (drop 4 cs)
  | "pi" `isPrefixOf` cs = TokenPi : lexer (drop 2 cs)
lexer (c:cs) = error ("lexer: unexpected character " ++ [c])

lexNum cs = TokenNumber (read num) : lexer rest
  where (num, rest) = span (\c -> isDigit c || c == '.') cs

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf l1 l2 = and $ zipWith (==) l1 l2

}