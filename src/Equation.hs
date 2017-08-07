module Equation where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust)

--------------------------------------------------------------------------------

type LHS = [Token]
type RHS = [Token]

type Input = String
type Tokens = [Token]

type Queue = [Token]
type Stack = [Token]

type Symbol = String
type Precedence = Int
type Associativity = Bool
type Arity = Int

--------------------------------------------------------------------------------

data Equation = Eqn LHS RHS -- Equation
              deriving (Eq, Show)

data Token = Con Constant -- Constant
           | Par Char     -- Parameter
           | Var Char     -- Variable
           | Opr Operator -- Operator
           | Fun Function -- Function
           | Sep          -- Separator
           | Opn          -- Open brackets
           | Cls          -- Close brackets
           | Not          -- Not a token
           deriving (Eq, Show)

data Constant = Z Int     -- Integers
              | Q Int Int -- Rationals
              | R Double  -- Reals
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