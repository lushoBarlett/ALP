{
module Parser where
import AST
}

%name qcparser
%tokentype { Token }
%error { parseError }

%token
  var  { TokenVar $$ }
  '='  { TokenEquals }
  '|'  { TokenIdentity }
  '->' { TokenArrow }
  '('  { TokenLParen }
  ')'  { TokenRParen }

%left '|'
%right '->'
%nonassoc '='

%%

Decl :: { QC }
Decl : var '=' Expr { QCDeclaration $$ $3 }

Expr :: { QC }
Expr : Tensor
     | Expr '->' Expr { QCArrow $1 $3 }

Tensor :: { QC }
Tensor : Operator Tensor { QCTensor $1 $2 }

Operator :: { QC }
Operator : var { QCVariable $1 }
         | '|' { QCIdentity }
         | '(' Expr ')' { $2 }

{
data Token
  = TokenVar String
  | TokenEquals
  | TokenIdentity
  | TokenArrow
  | TokenLParen
  | TokenRParen
  | TokenEOF
  deriving Show

parseError :: [Token] -> a
parseError _ = error "Parse error"

lexer :: String -> [Token]
lexer [] = [TokenEOF]
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexVar (c:cs)
lexer ('=':cs) = TokenEquals : lexer cs
lexer ('|':cs) = TokenIdentity : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs

lexVar cs =
  case span isAlpha cs of
    (var,rest) -> TokenVar var : lexer rest
}