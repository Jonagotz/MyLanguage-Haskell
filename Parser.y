{
module Parser where 

import Lexer 
}

%name parser 
%tokentype { Token }
%error { parseError }

%left '||'           -- menor precedência
%left "&&"
%nonassoc '=='
%left '+' '-'
%left '*'            -- maior precedência
%right '!'

%token 
    num             { TokenNum $$ }
    true            { TokenTrue }
    false           { TokenFalse }
    '!'             { TokenNot }
    '-'             { TokenSub }   
    '+'             { TokenPlus }
    '*'             { TokenTimes }
    "&&"            { TokenAnd }
    '||'            { TokenOr }
    '('             { TokenLParen }
    ')'             { TokenRParen }
    '['             { TokenLBracket }
    ']'             { TokenRBracket }
    if              { TokenIf }
    then            { TokenThen }
    else            { TokenElse }
    var             { TokenVar $$ }
    '\\'            { TokenLam }
    '='             { TokenAssign }
    Bool            { TokenBoolean }
    '=='            { TokenEquality }
    ':'             { TokenColon } 
    "->"            { TokenArrow }
    let             { TokenLet }
    in              { TokenIn }
    Num             { TokenNumber }
    str             { TokenString $$ }
    ','             { TokenComma }


%% 

Exp     : num                           { Num $1 }
        | str                           { Str $1 }  
        | true                          { BTrue }
        | false                         { BFalse }
        | '!' Exp                       { Not $2 }
        | Exp '[' Exp ']'               { Index $1 $3 }
        | Exp '==' Exp                  { Equality $1 $3 }
        | Exp '-' Exp                   { Sub $1 $3 }
        | Exp '+' Exp                   { Add $1 $3 }
        | Exp '*' Exp                   { Times $1 $3 }
        | '[' Args ']'                  { Array $2 }
        | Exp "&&" Exp                  { And $1 $3 }
        | Exp '||' Exp                  { Or $1 $3 }
        | '(' Exp ')'                   { Paren $2 }
        | var                           { Var $1 }
        | if Exp then Exp else Exp      { If $2 $4 $6 }
        | '\\' var ':' Type "->" Exp    { Lam $2 $4 $6 }
        | let var '=' Exp in Exp        { Let $2 $4 $6 }
        | Exp Exp                       { App $1 $2 }


Args    : Exp                       { [$1] }
| Exp ',' Args                      { $1 : $3 }
|                                   { [] }

Type    : Bool                          { TBool }
| Num                                   { TNum }
| '(' Type "->" Type ')'                { TFun $2 $4 }
| '[' Type ']'              { TArray $2 }

{ 

parseError :: [Token] -> a 
parseError _ = error "Syntax error!"

}