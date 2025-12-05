module Lexer where 

import Data.Char 

data Token = TokenNum Int 
           | TokenTrue 
           | TokenFalse
           | TokenPlus 
           | TokenColon
           | TokenSub
           | TokenTimes 
           | TokenAnd 
           | TokenOr 
           | TokenLParen 
           | TokenRParen 
           | TokenNot
           | TokenIf
           | TokenLam
           | TokenLBracket    
           | TokenRBracket   
           | TokenEquality -- ==
           | TokenThen
           | TokenElse
           | TokenString String
           | TokenLet
           | TokenAssign -- =
           | TokenIn
           | TokenBoolean
           | TokenComma
           | TokenNumber
           | TokenArrow
           | TokenVar String 
           deriving (Show, Eq)
-- AST
data Expr = Num Int 
          | Str String
          | Array [Expr]
          | Equality Expr Expr -- Para igualdade
          | Index Expr Expr -- Para acessar indexação do array
          | Let String Expr Expr -- Atribuição let x = 1 in x
          | BTrue 
          | BFalse 
          | Not Expr
          | Add Expr Expr 
          | Times Expr Expr 
          | And Expr Expr 
          | Or Expr Expr 
          | Paren Expr 
          | If Expr Expr Expr 
          | Var String
          | Lam String Ty Expr 
          | Sub Expr Expr
          | App Expr Expr 
          deriving (Show, Eq)

data Ty = TNum 
        | TBool 
        | TString
        | TArray Ty
        | TFun Ty Ty 
        deriving (Show, Eq) 
        
isSymb :: Char -> Bool
isSymb c = c `elem` "!+*()&|\\=[]:->,"

lexer :: String -> [Token]
lexer [] = []
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs
lexer ('"':cs) = lexString cs 
lexer (c:cs) | isSpace c = lexer cs 
             | isDigit c = lexNum (c:cs)
             | isSymb c = lexSymbol (c:cs)
             | isAlpha c = lexKw (c:cs)
lexer _ = error "Lexical error!"

lexNum cs = case span isDigit cs of 
              (num, rest) -> TokenNum (read num) : lexer rest 

lexSymbol :: String -> [Token]
lexSymbol cs = case span isSymb cs of
                 ("!", rest) -> TokenNot : lexer rest
                 ("-", rest) -> TokenSub : lexer rest
                 ("+", rest) -> TokenPlus : lexer rest
                 ("*", rest) -> TokenTimes : lexer rest
                 ("&&", rest) -> TokenAnd : lexer rest
                 ("||", rest) -> TokenOr : lexer rest
                 ("==", rest) -> TokenEquality : lexer rest
                 ("=", rest) -> TokenAssign : lexer rest
                 ("[", rest) -> TokenLBracket : lexer rest
                 ("->", rest) -> TokenArrow : lexer rest 
                 ("]", rest) -> TokenRBracket : lexer rest
                 (":", rest)  -> TokenColon : lexer rest 
                 ("\\", rest) -> TokenLam : lexer rest
                 (",", rest) -> TokenComma : lexer rest
                 _ -> error "Lexical error: invalid symbol!"
                 
lexString cs = case span (/= '"') cs of
    (str, '"':rest) -> TokenString str : lexer rest
    _ -> error "String sem fechamento de aspas"
                 
lexKw cs = case span isAlpha cs of 
             ("true", rest) -> TokenTrue : lexer rest 
             ("false", rest) -> TokenFalse : lexer rest 
             ("if", rest) -> TokenIf : lexer rest 
             ("then", rest) -> TokenThen : lexer rest 
             ("else", rest) -> TokenElse : lexer rest 
             ("let", rest) -> TokenLet : lexer rest 
             ("in", rest) -> TokenIn : lexer rest
             ("Num", rest) -> TokenNumber : lexer rest 
             ("Bool", rest) -> TokenBoolean : lexer rest 
             (var, rest) -> TokenVar var : lexer rest 