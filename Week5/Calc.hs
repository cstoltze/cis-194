module Calc where

import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + eval y
eval (Mul x y) = (eval x) * eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = evalMaybeExpr . parseExp Lit Add Mul

evalMaybeExpr :: Maybe ExprT -> Maybe Integer
evalMaybeExpr Nothing = Nothing
evalMaybeExpr (Just x) = Just $ eval x
