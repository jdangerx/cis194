{-# LANGUAGE FlexibleInstances #-}
module Calc where
import ExprT
import Parser
import qualified Data.Map as M

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
              Nothing -> Nothing
              Just expr -> Just (eval expr)

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- Exercise 4
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr Integer where
    lit = id
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit x = x > 0
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit n = MinMax n
    add (MinMax x) (MinMax y) = lit (max x y)
    mul (MinMax x) (MinMax y) = lit (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit n = Mod7 (n `mod` 7)
    add (Mod7 x) (Mod7 y) = lit (x + y)
    mul (Mod7 x) (Mod7 y) = lit (x * y)

-- Exercise 6
class HasVars a where
    var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = VLit
    add = VAdd
    mul = VMul

instance HasVars VarExprT where
    var = Var
