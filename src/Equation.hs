module Equation where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Ratio ((%), numerator, denominator)

--------------------------------------------------------------------------------

-- Equation types
type LHS = [Token]
type RHS = [Token]
type Symbol = String
type Precedence = Int
type Associativity = Bool
type Arity = Int
type BinaryOperation = Numeric -> Numeric -> Numeric

-- Lexer types
type Input = String
type Tokens = [Token]

-- Parser types
type TokenQueue = [Token]
type TokenStack = [Token]
type ExpressionStack = [Expression]
type Environment = [Char]

--------------------------------------------------------------------------------

-- Equation data type
data Equation = Eqn Expression Expression -- Equation
              deriving (Eq, Show)

--------------------------------------------------------------------------------

-- Token data type for lexer
data Token = Num Numeric -- Numeric
           | Chr Char -- Character
           | Opr Operator -- Operator
           | Fun Function -- Function
           | Sep -- Separator
           | Opn -- Open brackets
           | Cls -- Close brackets
           deriving (Eq, Show)

--------------------------------------------------------------------------------

-- Expression data type for parser
data Expression = Val Numeric -- Value
                | Con Char -- Constant
                | Var Char -- Variable
                | Bin Operator Expression Expression -- Binary Operator
                | App Function [Expression] -- Function Application
                deriving (Eq, Show)

-- Makes expression an instance of floating
instance Floating Expression where
  pi = Val (R pi)
  exp = Bin Exp (Val (R (exp 1)))
  log = App Log . return
  sin = App Sin . return
  cos = App Cos . return
  asin = App ASin . return
  acos = App ACos . return
  atan = App ATan . return
  sinh = App SinH . return
  cosh = App CosH . return
  asinh = App ASinH . return
  acosh = App ACosH . return
  atanh = App ATanH . return

-- Makes expression an instance of fractional
instance Fractional Expression where
  fromRational = Val . toQ . fromRational
  recip (Val val) = Val (recip val)
  recip exp = Bin Div (Val (Z 1)) exp

-- Makes expression an instance of num
instance Num Expression where
  (+) = Bin Add
  (*) = Bin Mul
  abs = App Abs . return
  signum = App Sgn . return
  fromInteger = Val . Z . fromIntegral
  negate = App Neg . return

--------------------------------------------------------------------------------

-- Numeric data type
data Numeric = Z Int -- Integers
             | Q Int Int -- Rationals
             | R Double -- Reals
             deriving Show

-- Makes numeric an instance of eq
instance Eq Numeric where
  Z z == Z z' = z == z'
  Z z == q @ (Q _ _) = fromIntegral z == fromQ q
  Z z == R r = fromIntegral z == r
  q @ (Q _ _) == Z z = fromQ q == fromIntegral z
  q @ (Q _ _) == q' @ (Q _ _) = fromQ q == fromQ q'
  q @ (Q _ _) == R r = fromRational (fromQ q) == r
  R r == Z z = r == fromIntegral z
  R r == q @ (Q _ _) = r == fromRational (fromQ q)
  R r == R r' = r == r'

-- Makes numeric an instance of floating
instance Floating Numeric where
  pi = R pi
  exp = R . applyDouble exp
  log = R . applyDouble log
  sin = R . applyDouble sin
  cos = R . applyDouble cos
  asin = R . applyDouble asin
  acos = R . applyDouble acos
  atan = R . applyDouble atan
  sinh = R . applyDouble sinh
  cosh = R . applyDouble cosh
  asinh = R . applyDouble asinh
  acosh = R . applyDouble acosh
  atanh = R . applyDouble atanh

-- Makes numeric an instance of fractional
instance Fractional Numeric where
  fromRational = toQ . fromRational
  recip (Z z) = toQ (recip (fromIntegral z))
  recip q @ (Q _ _) = toQ (recip (fromQ q))
  recip (R r) = R (recip r)

-- Makes numeric an instance of num
instance Num Numeric where
  Z z + Z z' = Z (z + z')
  Z z + q @ (Q _ _) = toQ (fromIntegral z + fromQ q)
  Z z + R r = R (fromIntegral z + r)
  q @ (Q _ _) + Z z = toQ (fromQ q + fromIntegral z)
  q @ (Q _ _) + q' @ (Q _ _) = toQ (fromQ q + fromQ q')
  q @ (Q _ _) + R r = R (fromRational (fromQ q) + r)
  R r + Z z = R (r + fromIntegral z)
  R r + q @ (Q _ _) = R (r + fromRational (fromQ q))
  R r + R r' = R (r + r')
  Z z * Z z' = Z (z * z')
  Z z * q @ (Q _ _) = toQ (fromIntegral z * fromQ q)
  Z z * R r = R (fromIntegral z * r)
  q @ (Q _ _) * Z z = toQ (fromQ q * fromIntegral z)
  q @ (Q _ _) * q' @ (Q _ _) = toQ (fromQ q * fromQ q')
  q @ (Q _ _) * R r = R (fromRational (fromQ q) * r)
  R r * Z z = R (r * fromIntegral z)
  R r * q @ (Q _ _) = R (r * fromRational (fromQ q))
  R r * R r' = R (r * r')
  abs (Z z) = Z (abs z)
  abs q @ (Q _ _) = toQ (abs (fromQ q))
  abs (R r) = R (abs r)
  signum (Z z) = Z (signum z)
  signum q @ (Q _ _) = toQ (signum (fromQ q))
  signum (R r) = R (signum r)
  fromInteger = Z . fromIntegral
  negate (Z z) = Z (negate z)
  negate q @ (Q _ _) = toQ (negate (fromQ q))
  negate (R r) = R (negate r)

--------------------------------------------------------------------------------

-- Operator enumeration
data Operator = Add -- Addition
              | Sub -- Subtraction
              | Mul -- Multiplication
              | Div -- Division
              | Exp -- Exponentiation
              | Jux -- Juxtaposition
              deriving (Eq, Show)

-- Gets the symbol of an operator
getOperatorSymbol :: Operator -> Symbol
getOperatorSymbol = fst . lookupOperator

-- Gets the precedence of an operator
getPrecedence :: Operator -> Precedence
getPrecedence = fst . snd . lookupOperator

-- Gets the associativity of an operator
getAssociativity :: Operator -> Associativity
getAssociativity = fst . snd . snd . lookupOperator

-- Gets the binary operation of an operator
getBinaryOperation :: Operator -> BinaryOperation
getBinaryOperation = fst . snd . snd . snd . lookupOperator

-- Looks up an operator
lookupOperator :: Operator -> (Symbol,
  (Precedence, (Associativity, (BinaryOperation, ()))))
lookupOperator = fromJust . flip lookup operatorTable

-- A lookup table for operators
operatorTable :: [(Operator, (Symbol,
  (Precedence, (Associativity, (BinaryOperation, ())))))]
operatorTable = map ($ ())
  [ (,) Add . (,) "+" . (,) 6 . (,) True . (,) (+)
  , (,) Sub . (,) "-" . (,) 6 . (,) True . (,) (-)
  , (,) Mul . (,) "*" . (,) 7 . (,) True . (,) (*)
  , (,) Div . (,) "/" . (,) 7 . (,) True . (,) (/)
  , (,) Exp . (,) "^" . (,) 8 . (,) False . (,) (.^.)
  , (,) Jux . (,) "." . (,) 10 . (,) True . (,) (*) ]

--------------------------------------------------------------------------------

-- Function enumeration
data Function = Log -- Logarithm
              | Abs -- Absolute
              | Sgn -- Sign
              | Neg -- Negate
              | Sin -- Sine
              | Cos -- Cosine
              | ASin -- Inverse Sine
              | ACos -- Inverse Cosine
              | ATan -- Inverse Tangent
              | SinH -- Hyperbolic Sine
              | CosH -- Hyperbolic Cosine
              | ASinH -- Inverse Hyperbolic Sine
              | ACosH -- Inverse Hyperbolic Cosine
              | ATanH -- Inverse Hyperbolic Tangent
              deriving (Eq, Show)

-- Gets the symbol of a function
getFunctionSymbol :: Function -> Symbol
getFunctionSymbol = fst . fromJust . flip lookup functionTable

-- Gets the arity of a function
getArity :: Function -> Arity
getArity = fst . snd . fromJust . flip lookup functionTable

-- Looks up a function
lookupFunction :: Function -> (Symbol, (Arity, ()))
lookupFunction = fromJust . flip lookup functionTable

-- A lookup table for functions
functionTable :: [(Function, (Symbol, (Arity, ())))]
functionTable = map ($ ())
  [ (,) Log . (,) "log" . (,) 2
  , (,) Abs . (,) "abs" . (,) 1
  , (,) Sgn . (,) "sgn" . (,) 1
  , (,) Neg . (,) "neg" . (,) 1
  , (,) ASin . (,) "asin" . (,) 1
  , (,) ACos . (,) "acos" . (,) 1
  , (,) ATan . (,) "atan" . (,) 1
  , (,) SinH . (,) "sinh" . (,) 1
  , (,) CosH . (,) "cosh" . (,) 1
  , (,) ASinH . (,) "asinh" . (,) 1
  , (,) ACosH . (,) "acosh" . (,) 1
  , (,) ATanH . (,) "atanh" . (,) 1 ]

--------------------------------------------------------------------------------

-- Converts a Q into a rational
fromQ :: Numeric -> Rational
fromQ (Z z) = fromIntegral z % 1
fromQ (Q n d) = fromIntegral n % fromIntegral d
fromQ (R r) = approxQ (r, 1)

-- Approximates a Q from a double
approxQ :: (Double, Integer) -> Rational
approxQ (n, d)
  | n == fromIntegral (floor n) = floor n % d
  | otherwise = approxQ (10 * n, 10 * d)

-- Converts a rational into a Q
toQ :: Rational -> Numeric
toQ = Q . fromIntegral . numerator <*> fromIntegral . denominator

-- Real exponentiation
infixr 8 .^.
(.^.) :: Numeric -> Numeric -> Numeric
val .^. val' = R (applyDouble (flip applyDouble val . flip (**)) val')

-- Applies a double to double function onto a numeric
applyDouble :: (Double -> Double) -> Numeric -> Double
applyDouble function (Z z) = function (fromIntegral z)
applyDouble function q @ (Q _ _) = function (fromRational (fromQ q))
applyDouble function (R r) = function r