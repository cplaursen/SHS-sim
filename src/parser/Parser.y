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
    "real"   { L _ (TokenIdent "real") }
    "bool"   { L _ (TokenIdent "bool") }
    "int"    { L _ (TokenIdent "int") }
    "enum"   { L _ (TokenIdent "enum") }
    dW       { L _ TokenDW }
    dt       { L _ TokenDT }
    skip     { L _ TokenSkip }
    abort    { L _ TokenAbort }
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
    "++"     { L _ TokenUnion }
    "?"      { L _ TokenQuestion }
    or       { L _ TokenOr }
    and      { L _ TokenAnd }
    "~"      { L _ TokenNot }
    "+"      { L _ TokenPlus }
    "-"      { L _ TokenMinus }
    ">"      { L _ TokenGT }
    "<"      { L _ TokenLT }
    ">="     { L _ TokenGEQ }
    "<="     { L _ TokenLEQ }
    "="      { L _ TokenEq }
    "/"      { L _ TokenDiv }

%nonassoc "<" ">" "<=" ">=" ":=" "="

%right then else
%left ";"
%left "++"
%left or
%left and
%left "+" "-"
%left "*" "/"
%left NEG

%%

SHPProg : Blocks { $1 }

Blocks :: { Blocks }
Blocks : {- Empty -} { emptyBlock }
       | Blocks "SHP" "{" SHP "}" { $1 & shpBlock .~ $4 }
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

Vars :: { [(String, PSHPType, Maybe PExpr)] }
Vars : {- Empty -} { [] }
     |  Vars Var { $2 : $1 }

Var :: { (String, PSHPType, Maybe PExpr) }
Var : Typedecl id "=" Expr ";" { ($2, $1, Just $4) }
    | Typedecl id ";" { ($2, $1, Nothing) }

Typedecl :: { PSHPType }
Typedecl : "real" { HPReal }
         | "bool" { HPBool }
         | "int"  { HPInt }
         | "enum" { HPEnum }

SHP :: { PSHP }
SHP : {- Empty -} { PSkip }
    | SHP ";" SHP   { PComp $1 $3 } 
    | SHP "++" SHP { PChoice (PReal 0.5) $1 $3 } -- Fix this syntax
    | if Expr then SHP else SHP  { PCond $2 $4 $6 }
    | if Expr then SHP { PCond $2 $4 PSkip }
    | "?" Expr { PCond $2 PSkip PAbort }
    | "(" SHP ")" { $2 }
    | while Expr "{" SHP "}" { PWhile $2 $4 }
    | id ":=" "{" Expr "," Expr "}" { PRandAssn $1 $4 $6 }
    | id ":=" Expr { PAssn $1 $3 }
    | input id { PInput $2 }
    | abort { PAbort }
    | skip { PSkip }
    | SDE { $1 }

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
    | Expr "+" Expr { PBop "+" $1 $3 }
    | Expr "-" Expr { PBop "-" $1 $3 }
    | Expr "*" Expr { PBop "*" $1 $3 }
    | Expr "/" Expr { PBop "/" $1 $3 }
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

parseError :: Lexeme -> Alex a
parseError _ = alexError "Parse error"

emptyBlock :: Blocks
emptyBlock = Blocks PSkip [] [] []

main = do
    s <- getContents
    print $ runAlex s parseSHPProg
}
