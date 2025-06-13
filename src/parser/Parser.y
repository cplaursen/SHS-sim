{
module Parser ( parseExpr, parseSHPProg ) where
import Lexer
import ParsingTypes
import Lens.Micro.Platform ((&), (%~), (.~))

}

%name parseExpr Expr
%name parseSHPProg SHPProg
%tokentype { Lexeme }
%error { parseError }

%monad { Alex }
%lexer { (alexMonadScan >>=) } { L _ TokenEOF }

%token
    "SHP"    { L _ (TokenIdent "SHP")}
    "Enums"   { L _ (TokenIdent "Enums")}
    "Constants" { L _ (TokenIdent "Constants")}
    "Variables" { L _ (TokenIdent "Variables") }
    "Options" { L _ (TokenIdent "Options") }
    "real"   { L _ (TokenIdent "real") }
    "bool"   { L _ (TokenIdent "bool") }
    "int"    { L _ (TokenIdent "int") }
    "enum"   { L _ (TokenIdent "enum") }
    dW       { L _ TokenDW }
    dt       { L _ TokenDT }
    input    { L _ TokenInput }
    if       { L _ TokenIf }
    then     { L _ TokenThen }
    else     { L _ TokenElse }
    while    { L _ TokenWhile }
    id       { L _ (TokenIdent $$) }
    "("      { L _ TokenLParen }
    ")"      { L _ TokenRParen }
    real     { L _ (TokenReal $$) }
    bool     { L _ (TokenBool $$) }
    "'"      { L _ TokenPrime }
    ","      { L _ TokenComma }
    "{"      { L _ TokenLCurl }
    "}"      { L _ TokenRCurl }
    ";"      { L _ TokenSemi } 
    "&"      { L _ TokenAmpersand }
    ":="     { L _ TokenAssign }
    "*"      { L _ TokenStar  }
    "?"      { L _ TokenQuestion }
    or       { L _ TokenOr }
    and      { L _ TokenAnd }
    sin      { L _ TokenSin }
    cos      { L _ TokenCos }
    "~"      { L _ TokenNot }
    "+"      { L _ TokenPlus }
    "-"      { L _ TokenMinus }
    "^"      { L _ TokenPow }
    ">"      { L _ TokenGT }
    "<"      { L _ TokenLT }
    ">="     { L _ TokenGEQ }
    "<="     { L _ TokenLEQ }
    "="      { L _ TokenEq }
    "/"      { L _ TokenDiv }

%left or
%left and

%nonassoc "<" ">" "<=" ">=" ":=" "=" "^"

%left "+" "-"
%left "*" "/"
%left NEG


%%

SHPProg : Blocks { $1 }

Blocks :: { Blocks }
Blocks : {- Empty -} { emptyBlock }
       | Blocks "SHP" "{" SHPLines "}" { $1 & shpBlock .~ (linesToSHP $4) }
       | Blocks "Constants" "{" Definitions "}" { $1 & constBlock %~ (++ reverse $4) }
       | Blocks "Enums" "{" Ids "}" { $1 & enumBlock %~ (++ reverse $4) }
       | Blocks "Variables" "{" Vars "}" { $1 & varsBlock %~ (++ reverse $4) }

Definitions :: { [Def] }
Definitions : {- Empty -} { [] }
            | Definitions Definition { $2 : $1 }

Definition :: { Def }
Definition : id "=" Expr ";" { Def $1 $3 }

Ids :: { [String] }
Ids : id { [$1] }
    | Ids "," id { $3 : $1 }

Vars :: { [Either ([String], PSHPType) (String, PSHPType, PExpr)] }
Vars : {- Empty -} { [] }
     |  Vars Var { $2 : $1 }

Var :: { Either ([String], PSHPType) (String, PSHPType, PExpr) }
Var : Typedecl id "=" Expr ";" { Right ($2, $1, $4) }
    | Typedecl id ";" { Left ([$2], $1) }
    | Typedecl Ids ";" { Left ($2, $1) }

Typedecl :: { PSHPType }
Typedecl : "real" { HPReal }
         | "bool" { HPBool }
         | "int"  { HPInt }
         | "enum" { HPEnum }

SHPLines :: { [PSHP] }
SHPLines : SHPLine { [$1] }
         | SHPLines SHPLine { $2 : $1 }

SHPLine :: { PSHP }
SHPLine : {- Empty -} { PSkip }
       -- Maybe add an Expr field to control probability
    | if Expr "{" SHPLines "}" else "{" SHPLines "}" { PCond $2 (linesToSHP $4) (linesToSHP $8) }
    | if Expr "{" SHPLines "}" { PCond $2 (linesToSHP $4) PSkip }
    | while Expr "{" SHPLines "}" { PWhile $2 (linesToSHP $4) }
    | id ":=" "{" Expr "," Expr "}" ";" { PRandAssn $1 $4 $6 }
    | id ":=" Expr ";" { PAssn $1 $3 }
    | input id ";" { PInput $2 }
    | SDE ";" { $1 }

SDE :: { PSHP }
SDE : Drift "&" Expr { PSDE $1 [] $3 }
    | Volatility "&" Expr { PSDE [] $1 $3 }
    | Drift "+" Volatility "&" Expr { PSDE $1 $3 $5 }

Drift :: { [PDiff] }
Drift : "{" Differentials "}" dt { $2 }

Volatility :: { [PDiff] }
Volatility : "{" Differentials "}" dW { $2 }

Diff :: { PDiff }
Diff : id "'" "=" Expr { PDiff $1 $4 }

Differentials :: { [PDiff] }
Differentials : Diff { [$1] }
  | Differentials "," Diff { $3 : $1 }

Expr :: { PExpr }
Expr : real         { PReal $1 }
    | id            { PVar $1 }
    | bool          { PBool $1 }
    | sin "(" Expr ")" { PUop "sin" $3 }
    | cos "(" Expr ")" { PUop "cos" $3 }
    | Expr "^" Expr { PBop "^" $1 $3 }
    | Expr "/" Expr { PBop "/" $1 $3 }
    | Expr "*" Expr { PBop "*" $1 $3 }
    | Expr "+" Expr { PBop "+" $1 $3 }
    | Expr "-" Expr { PBop "-" $1 $3 }
    | "-" Expr %prec NEG { PUop "-" $2 }
    | Expr "=" Expr  { PBop "=" $1 $3 }
    | Expr ">" Expr  { PBop ">" $1 $3 }
    | Expr "<" Expr  { PBop "<" $1 $3 }
    | Expr ">=" Expr { PBop ">=" $1 $3 }
    | Expr "<=" Expr { PBop "<=" $1 $3 }
    | Expr or Expr   { PBop "||" $1 $3 }
    | Expr and Expr  { PBop "&&" $1 $3 }
    | "~" Expr       { PUop "~" $2 }
    | "(" Expr ")"  { $2 }

{ 

-- Parser only returns non-empty lists of lines
linesToSHP :: [PSHP] -> PSHP
linesToSHP [] = error "Called linesToSHP on empty list, something has gone wrong in the parser"
linesToSHP (x:[]) = x
linesToSHP (x:xs) = PComp (linesToSHP xs) x

parseError :: Lexeme -> Alex a
parseError (L (AlexPn _ line col) tok) = alexError $ "Parse error at line " ++ (show line) ++ ", column " ++ (show col) ++ ": unexpected token " ++ show tok

emptyBlock :: Blocks
emptyBlock = Blocks PSkip [] [] []

main = do
    s <- getContents
    print $ runAlex s parseSHPProg
}
