{-# LANGUAGE FlexibleInstances #-}

import Control.Monad
import Data.Maybe
import ExprT
import Parser
import qualified Data.Map as M


-- Exercise #1

eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add a b) = (+) (eval a) (eval b)
eval (Mul a b) = (*) (eval a) (eval b)


-- Exercise #2

evalStr :: String -> Maybe Integer
evalStr expression
  | isJust parsed = Just (eval $ fromJust parsed)
  | otherwise     = Nothing
  where parsed = parseExp Lit Add Mul expression


-- Exercise #3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x   = Lit x
  add x y = Add x y
  mul x y = Mul x y


-- Exercse #4

instance Expr Integer where
  lit x   = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x   | x > 0     = True
          | otherwise = False
  add x y = x || y
  mul x y = x && y

newtype MinMax = MinMax Integer deriving (Show, Eq)

instance Expr MinMax where
  lit x                     = MinMax x
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr Mod7 where
  lit x                 = Mod7 x
  add (Mod7 x) (Mod7 y) = Mod7 ( (x + y) `mod` 7 )
  mul (Mod7 x) (Mod7 y) = Mod7 ( (x + y) `mod` 7 )


-- Exercise #5


-- Exercise #6

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | VVar String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit x   = VLit x
  add x y = VAdd x y
  mul x y = VMul x y

instance HasVars VarExprT where
  var x = VVar x

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var x = M.lookup x

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x       = \myMap -> Just x
  add fn1 fn2 = \myMap -> (liftM2 (+)) (fn1 myMap) (fn2 myMap)
  mul fn1 fn2 = \myMap -> (liftM2 (*)) (fn1 myMap) (fn2 myMap)


withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
