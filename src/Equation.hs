module Equation where

import Data.List
import Data.Function

--------------------------------------------------------------------------------

data Equation = Eqn [Token] [Token] -- Equation
              deriving (Eq, Show)

data Token = Con Constant -- Constant
           | Var Char     -- Variable
           | Opr Operator -- Operator
           | Opn          -- Open brackets
           | Cls          -- Close brackets
           | Not          -- Not a token
           deriving (Eq, Show)

data Constant = Z Int     -- Integers
              | Q Int Int -- Rationals
              | R Double  -- Reals
              deriving (Eq, Show)

data Operator = Add -- Addition (+)
              | Sub -- Subtraction (-)
              | Mul -- Multiplication (*)
              | Div -- Division (/)
              | Exp -- Exponentiation (^)
              | Log -- Logarithm (log)
              deriving (Eq, Show)

--------------------------------------------------------------------------------

operators :: [Operator]
operators = [Add, Sub, Mul, Div, Exp, Log]

operators' :: [String]
operators' = ["+", "-", "*", "/", "^", "log"]

lookupTable :: [(Operator, String)]
lookupTable = sortBy (on compare (length . snd)) (zip operators operators')