{
module Lexer where

import ParsingTypes
}

%wrapper "monad"

$digit = [0-9]
@id         = [A-Za-z][A-Za-z_]*

tokens :-
    $white+             { skip }
    "//".*              { skip }
    dW                  { mkL TokenDW     }
    dt                  { mkL TokenDT     }
    abort               { mkL TokenAbort } 
    skip                { mkL TokenSkip } 
    input               { mkL TokenInput } 
    if                  { mkL TokenIf }
    then                { mkL TokenThen }
    else                { mkL TokenElse } 
    while               { mkL TokenWhile }
    true                { mkL $ TokenBool True }
    false               { mkL $ TokenBool False }
    \+\+                { mkL TokenUnion }
    :=                  { mkL TokenAssign } 
    \\\/                { mkL TokenOr }
    \/\\                { mkL TokenAnd }
    \~                  { mkL TokenNot }
    \<=                 { mkL TokenLEQ }
    >=                  { mkL TokenGEQ }
    \<                  { mkL TokenLT }
    \>                  { mkL TokenGT }
    =                   { mkL TokenEq }
    \+                  { mkL TokenPlus }
    \-                  { mkL TokenMinus }
    \/                  { mkL TokenDiv }
    \(                  { mkL TokenLParen }
    \)                  { mkL TokenRParen }
    \{                  { mkL TokenLCurl } 
    \}                  { mkL TokenRCurl } 
    \,                  { mkL TokenComma }
    \;                  { mkL TokenSemi } 
    \&                  { mkL TokenAmpersand } 
    \*                  { mkL TokenStar } 
    \?                  { mkL TokenQuestion }
    '                   { mkL TokenPrime }
    $digit+(\.$digit+)? { mkL_input (TokenReal . read) }
    @id                 { mkL_input TokenIdent }
{

data Lexeme = L AlexPosn Token
    deriving (Eq, Show)

mkL :: Token -> AlexAction Lexeme
mkL t (p, _, _, str) len = return (L p t)

mkL_input :: (String -> Token) -> AlexAction Lexeme
mkL_input t (p, _, _, str) len = return (L p (t (take len str)))

alexEOF :: Alex Lexeme
alexEOF = return (L undefined TokenEOF)

}
