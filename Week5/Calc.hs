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

-- Exercise 3
class Expr a where
    lit :: Integer -> a 
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit x = Lit x
    add x y = Add x y
    mul x y = Mul x y

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit x = x > 0
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit x = MinMax x
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7    = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit x = Mod7 x
    add (Mod7 x) (Mod7 y) = Mod7 $ ((x `mod` 7) + (y `mod` 7)) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ ((x `mod` 7) * (y `mod` 7)) `mod` 7

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul

testInteger  = testExp :: String -> Maybe Integer
testBool     = testExp :: String -> Maybe Bool
testMM       = testExp :: String -> Maybe MinMax
testSat      = testExp :: String -> Maybe Mod7
