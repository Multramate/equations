module Equation where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust)

--------------------------------------------------------------------------------

-- Equation
type LHS = [Token]
type RHS = [Token]
type Symbol = String
type Precedence = Int
type Associativity = Bool
type Arity = Int

-- Lexer
type Input = String
type Tokens = [Token]

-- Parser
type TokenQueue = [Token]
type TokenStack = [Token]
type ExpressionStack = [Expression]
type Environment = [Char]

--------------------------------------------------------------------------------

data Equation = Eqn Expression Expression -- Equation
              deriving (Eq, Show)

data Token = Num Numeric -- Numeric
           | Chr Char -- Character
           | Opr Operator -- Operator
           | Fun Function -- Function
           | Sep -- Separator
           | Opn -- Open brackets
           | Cls -- Close brackets
           deriving (Eq, Show)

data Expression = Val Numeric -- Value
                | Con Char -- Constant
                | Var Char -- Variable
                | Bin Operator Expression Expression -- Binary Operator
                | App Function [Expression] -- Function Application
                deriving (Eq, Show)

data Numeric = Z Int -- Integers
             | Q Int Int -- Rationals
             | R Double -- Reals
             deriving (Eq, Show)

data Operator = Add -- Addition
              | Sub -- Subtraction
              | Mul -- Multiplication
              | Div -- Division
              | Exp -- Exponentiation
              | Jux -- Juxtaposition
              deriving (Eq, Show)

data Function = Log -- Logarithm
              deriving (Eq, Show)

--------------------------------------------------------------------------------

getOperator :: Operator -> Symbol
getOperator = fst . fromJust . flip lookup operatorTable

getPrecedence :: Operator -> Precedence
getPrecedence = fst . snd . fromJust . flip lookup operatorTable

getAssociativity :: Operator -> Associativity
getAssociativity = fst . snd . snd . fromJust . flip lookup operatorTable

getFunction :: Function -> Symbol
getFunction = fst . fromJust . flip lookup functionTable

getArity :: Function -> Arity
getArity = fst . snd . fromJust . flip lookup functionTable

operatorTable :: [(Operator, (Symbol, (Precedence, (Associativity, ()))))]
operatorTable = map ($ ())
  [ (,) Add . (,) "+" . (,) 6 . (,) True
  , (,) Sub . (,) "-" . (,) 6 . (,) True
  , (,) Mul . (,) "*" . (,) 7 . (,) True
  , (,) Div . (,) "/" . (,) 7 . (,) True
  , (,) Exp . (,) "^" . (,) 8 . (,) False
  , (,) Jux . (,) "." . (,) 10 . (,) True ]

functionTable :: [(Function, (Symbol, (Arity, ())))]
functionTable = map ($ ())
  [ (,) Log . (,) "log" . (,) 2 ]