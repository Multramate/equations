module Equation where

import Data.List
import Data.Function

--------------------------------------------------------------------------------

type Input = String

type Queue = [Token]
type Stack = [Token]
type Tokens = [Token]

type LHS = [Token]
type RHS = [Token]

data Equation = Eqn LHS RHS -- Equation
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

symbolTable :: [(Operator, String)]
symbolTable = sortBy (on compare (length . snd)) (zip operators symbols)

symbols :: [String]
symbols = ["+", "-", "*", "/", "^", "log"]

precedenceTable :: [(Operator, (Int, Bool))]
precedenceTable = zip operators (zip precedences leftAssocs)

precedences :: [Int]
precedences = [6, 6, 7, 7, 8, 10]

leftAssocs :: [Bool]
leftAssocs = [True, True, True, True, False, True]