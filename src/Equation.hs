module Equation where

import Data.List
import Data.Function

--------------------------------------------------------------------------------

data Equation = Eqn [Token] [Token] -- Equation
              deriving (Eq, Show)

data Token = Con Constant           -- Constant
           | Var Char               -- Variable
           | Opr Operator           -- Operator
           | Opn                    -- Open brackets
           | Cls                    -- Close brackets
           deriving (Eq, Show)

data Constant = Z Int               -- Integers
              | Q (Int, Int)        -- Rational numbers
              | R Double            -- Real numbers
              | C (Double, Double)  -- Complex Numbers
              deriving (Eq, Show)

data Operator = Add                 -- Addition (+)
              | Sub                 -- Subtraction (-)
              | Mul                 -- Multiplication (*)
              | Div                 -- Division (/)
              | Exp                 -- Exponentiation (^)
              | Log                 -- Logarithm (log)
              deriving (Eq, Show)

--------------------------------------------------------------------------------

tokens :: [Token]
tokens = [Opr Add, Opr Sub, Opr Mul, Opr Div, Opr Exp, Opr Log, Opn, Cls]

operators :: [String]
operators = ["+", "-", "*", "/", "^", "log", "(", ")"]

lookupTable :: [(Token, String)]
lookupTable = sortBy (on compare (length . snd)) (zip tokens operators)