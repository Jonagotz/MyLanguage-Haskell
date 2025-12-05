module Interpreter where 

import Lexer 
import Parser 

isValue :: Expr -> Bool 
isValue BTrue  = True 
isValue BFalse = True
isValue (Num _) = True 
isValue (Lam _ _ _) = True 
isValue (Str _) = True
isValue (Array xs) = all isValue xs
isValue _ = False 
-- ADICIONADO ISVALUE PARA ARRAY

subst :: String -> Expr -> Expr -> Expr 
subst x s y@(Var v) = if x == v then 
                        s 
                      else 
                        y 
subst x s (Num n) = (Num n)
subst x s BTrue = BTrue 
subst x s BFalse = BFalse 
subst x s (Str str) = Str str
subst x s (Lam y tp t1) = Lam y tp (subst x s t1)
subst x s (Not t1) = Not (subst x s t1)
subst x s (App t1 t2) = App (subst x s t1) (subst x s t2) 
subst x s (Add t1 t2) = Add (subst x s t1) (subst x s t2) 
subst x s (Sub t1 t2) = Sub (subst x s t1) (subst x s t2)    
subst x s (And t1 t2) = And (subst x s t1) (subst x s t2) 
subst x s (If t1 t2 t3) = If (subst x s t1) (subst x s t2) (subst x s t3)
subst x s (Times t1 t2) = Times (subst x s t1) (subst x s t2)
subst x s (Or t1 t2) = Or (subst x s t1) (subst x s t2)
subst x s (Paren t1) = Paren (subst x s t1)
subst x s (Let x1 t1 t2) = Let x1 (subst x s t1) (subst x s t2)
subst x s (Equality t1 t2) = Equality (subst x s t1) (subst x s t2)
subst x s (Array xs) = Array (map (subst x s) xs)
subst x s (Index t1 t2) = Index (subst x s t1) (subst x s t2)
-- ADICIONADO SUBST PARA TIMES, OR, IF E PARENTESIS (PAREN, nao sei se é necessario)

step :: Expr -> Expr 

-- STEP PRO NOT
step (Not BTrue) = BFalse
step (Not BFalse) = BTrue
step (Not e) = Not (step e)

step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) = let e2' = step e2
                           in Add (Num n1) e2' 
step (Add e1 e2) = Add (step e1) e2 

-- STEP PRO SUB

step (Sub (Num n1) (Num n2)) = Num (n1 - n2)
step (Sub (Num n1) e2) = Sub (Num n1) (step e2)
step (Sub e1 e2) = Sub (step e1) e2

-- Implementar step para Times
step (Times (Num n1) (Num n2)) = Num (n1 * n2)
step (Times (Num n1) e2) = Times (Num n1) (step e2)
step (Times e1 e2) = Times (step e1) e2

step (And BFalse e2) = BFalse 
step (And BTrue e2) = e2 
step (And e1 e2) = And (step e1) e2 
-- Implementar step para Or
step (Or BTrue _) = BTrue
step (Or BFalse e2) = e2
step (Or e1 e2) = Or (step e1) e2
-- Implementar step para If
step (If BTrue e1 e2) = e1
step (If BFalse e1 e2) = e2
step (If e1 e2 e3) = If (step e1) e2 e3
-- Equality
step (Equality e1 e2) 
    | isValue e1 && isValue e2 = if e1 == e2 then BTrue else BFalse
    | isValue e1               = Equality e1 (step e2)
    | otherwise                = Equality (step e1) e2 
-- Let
step (Let x e1 e2) | isValue e1 = subst x e1 e2
                   | otherwise = Let x (step e1) e2
-- STEP PARA ARRAY

step (Array xs) 
    | all isValue xs = Array xs  -- Já é valor, não precisa step
    | otherwise = case break (not . isValue) xs of
        (before, e:after) -> Array (before ++ [step e] ++ after)
    
-- STEP PARA  indexacao
    
step (Index e1 e2) 
    | not (isValue e1) = Index (step e1) e2
    | not (isValue e2) = Index e1 (step e2)
    | otherwise = case (e1, e2) of
        (Array xs, Num i) -> 
            if i >= 0 && i < length xs 
            then xs !! i 
            else error $ "Index out of bounds: " ++ show i ++ " (array size: " ++ show   (length xs) ++ ")"
        _ -> error $ "Type error in index: cannot index " ++ show e1 ++ " with " ++ show e2

step (App (Lam x tp e1) e2) = if (isValue e2) then 
                                subst x e2 e1 
                              else 
                                App (Lam x tp e1) (step e2)

step (App e1 e2) = App (step e1) e2
-- step para paren
step (Paren e) = e

step e = error (show e)


eval :: Expr -> Expr
eval e = if isValue e then 
           e
         else 
           eval (step e)
