module TypeChecker where 

import Lexer 

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty 
typeof ctx BTrue = Just TBool 
typeof ctx BFalse = Just TBool 
typeof ctx (Num n) = Just TNum 
typeof ctx (Str s) = Just TString 
typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TNum, Just TNum) -> Just TNum 
                           _                      -> Nothing

-- ADICIONA SUB (MENOS)
typeof ctx (Sub e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                           (Just TNum, Just TNum) -> Just TNum
                           _ ->                      Nothing
                           
-- TYPEOF PARA TIMES IMPLEMENTADO ABAIXO
typeof ctx (Times e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TNum, Just TNum) -> Just TNum
                           _                      -> Nothing


typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TBool, Just TBool) -> Just TBool 
                           _                        -> Nothing
-- TYPEOF PARA OR IMPLEMENTADO ABAIXO
typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TBool, Just TBool) -> Just TBool
                           _                        -> Nothing
                           
typeof ctx (If e e1 e2) = case typeof ctx e of 
                            Just TBool -> case (typeof ctx e1, typeof ctx e2) of 
                                            (Just t1, Just t2) | t1 == t2  -> Just t1 
                                                               | otherwise -> Nothing 
                                            _ -> Nothing  
                            _ -> Nothing 
typeof ctx (Var x) = lookup x ctx 
typeof ctx (Lam x tp b) = let ctx' = (x,tp) : ctx 
                            in case (typeof ctx' b) of 
                                 Just tr -> Just (TFun tp tr)
                                 _ -> Nothing 
typeof ctx (App e1 e2) = case typeof ctx e1 of 
                           Just (TFun tp tr) -> case typeof ctx e2 of 
                                                  Just t2 | t2 == tp -> Just tr 
                                                  _ -> Nothing 
                           _ -> Nothing 

typeof ctx (Paren e) = typeof ctx e

-- TYPEOF PARA EQUALITY IMPLEMENTADO ABAIXO ==
typeof ctx (Equality e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                                (Just TNum, Just TNum) -> Just TBool 
                                (Just TBool, Just TBool) -> Just TBool
                                (Just TString, Just TString) -> Just TBool
                                _ -> Nothing

-- TYPEOF PARA LET IMPLEMENTADO ABAIXO
typeof ctx (Let x e1 e2) = case typeof ctx e1 of
                            Just tp -> typeof ((x, tp):ctx) e2
                            _ -> Nothing
-- INDEXACAO
typeof ctx (Index e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just (TArray t), Just TNum) -> Just t
  _ -> Nothing
-- TRATA ARRAY
typeof ctx (Array []) = Nothing
typeof ctx (Array (x:xs)) = case typeof ctx x of
  Just t -> if all (\x -> typeof ctx x == Just t) xs
            then Just (TArray t)           
            else Nothing
  _ ->                                      Nothing                                            
-- NOT OPERATOR
typeof ctx (Not e1) = case typeof ctx e1 of
                           Just TBool -> Just TBool
                           _ -> Nothing

typecheck :: Expr -> Expr 
typecheck e = case typeof [] e of 
                Just _ -> e 
                _      -> error "Type error!"
