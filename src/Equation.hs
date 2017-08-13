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
type FunctionApplication = [Numeric] -> Numeric

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
data Token = Num Numeric  -- Numeric
           | Chr Char     -- Character
           | Opr Operator -- Operator
           | Fun Function -- Function
           | Sep          -- Separator
           | Opn          -- Open brackets
           | Cls          -- Close brackets
           deriving (Eq, Show)

--------------------------------------------------------------------------------

-- Expression data type for parser
data Expression = Val Numeric                        -- Value
                | Con Char                           -- Constant
                | Var Char                           -- Variable
                | Bin Operator Expression Expression -- Binary Operation
                | App Function [Expression]          -- Function Application
                deriving (Eq, Ord, Show)

-- Makes expression an instance of floating
instance Floating Expression where
  pi = Val pi
  exp = Bin Exp (Val (exp 1))
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
  recip = App Rcp . return

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
data Numeric = Z Int     -- Integers
             | Q Int Int -- Rationals
             | R Double  -- Reals
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
  pi = pi
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
  
-- Makes numeric an instance of ord
instance Ord Numeric where
  Z z <= Z z' = z <= z'
  Z z <= q @ (Q _ _) = fromIntegral z <= fromQ q
  Z z <= R r = fromIntegral z <= r
  q @ (Q _ _) <= Z z = fromQ q <= fromIntegral z
  q @ (Q _ _) <= q' @ (Q _ _) = fromQ q <= fromQ q'
  q @ (Q _ _) <= R r = fromRational (fromQ q) <= r
  R r <= Z z = r <= fromIntegral z
  R r <= q @ (Q _ _) = r <= fromRational (fromQ q)
  R r <= R r' = r <= r'

--------------------------------------------------------------------------------

-- Operator enumeration
data Operator = Add -- Addition
              | Sub -- Subtraction
              | Mul -- Multiplication
              | Div -- Division
              | Exp -- Exponentiation
              | Jux -- Juxtaposition
              deriving (Eq, Show)

-- Makes operator an instance of ord
instance Ord Operator where
  compare _ _ = EQ

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
data Function = Sum   -- Sum
              | Prd   -- Product
              | Log   -- Logarithm
              | Neg   -- Negate
              | Rcp   -- Reciprocal
              | Abs   -- Absolute
              | Sgn   -- Sign
              | Sin   -- Sine
              | Cos   -- Cosine
              | ASin  -- Inverse Sine
              | ACos  -- Inverse Cosine
              | ATan  -- Inverse Tangent
              | SinH  -- Hyperbolic Sine
              | CosH  -- Hyperbolic Cosine
              | ASinH -- Inverse Hyperbolic Sine
              | ACosH -- Inverse Hyperbolic Cosine
              | ATanH -- Inverse Hyperbolic Tangent
              deriving (Eq, Show)

-- Makes function an instance of ord
instance Ord Function where
  compare _ _ = EQ

-- Gets the symbol of a function
getFunctionSymbol :: Function -> Symbol
getFunctionSymbol = fst . lookupFunction

-- Gets the arity of a function
getArity :: Function -> Arity
getArity = fst . snd . lookupFunction

-- Gets the function application of a function
getFunctionApplication :: Function -> FunctionApplication
getFunctionApplication = fst . snd . snd . lookupFunction

-- Looks up a function
lookupFunction :: Function -> (Symbol, (Arity, (FunctionApplication, ())))
lookupFunction = fromJust . flip lookup functionTable

-- A lookup table for functions
functionTable :: [(Function, (Symbol, (Arity, (FunctionApplication, ()))))]
functionTable = map ($ ())
  [ (,) Sum . (,) "sum" . (,) 0 . (,) sum
  , (,) Prd . (,) "prd" . (,) 0 . (,) product
  , (,) Log . (,) "log" . (,) 2 . (,) (applyBinary logBase)
  , (,) Neg . (,) "neg" . (,) 1 . (,) (applyUnary negate)
  , (,) Rcp . (,) "rcp" . (,) 1 . (,) (applyUnary recip)
  , (,) Abs . (,) "abs" . (,) 1 . (,) (applyUnary abs)
  , (,) Sgn . (,) "sgn" . (,) 1 . (,) (applyUnary signum)
  , (,) Sin . (,) "sin" . (,) 1 . (,) (applyUnary sin)
  , (,) Cos . (,) "cos" . (,) 1 . (,) (applyUnary cos)
  , (,) ASin . (,) "asin" . (,) 1 . (,) (applyUnary asin)
  , (,) ACos . (,) "acos" . (,) 1 . (,) (applyUnary acos)
  , (,) ATan . (,) "atan" . (,) 1 . (,) (applyUnary atan)
  , (,) SinH . (,) "sinh" . (,) 1 . (,) (applyUnary sinh)
  , (,) CosH . (,) "cosh" . (,) 1 . (,) (applyUnary cosh)
  , (,) ASinH . (,) "asinh" . (,) 1 . (,) (applyUnary asinh)
  , (,) ACosH . (,) "acosh" . (,) 1 . (,) (applyUnary acosh)
  , (,) ATanH . (,) "atanh" . (,) 1 . (,) (applyUnary atanh) ]

-- Applies a unary function on a list of numerics
applyUnary :: (Numeric -> Numeric) -> [Numeric] -> Numeric
applyUnary = (. head)

-- Applies a binary function on a list of numerics
applyBinary :: (Numeric -> Numeric -> Numeric) -> [Numeric] -> Numeric
applyBinary = flip (<*>) (head . tail) . (. head)

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
applyDouble fun (Z z) = fun (fromIntegral z)
applyDouble fun q @ (Q _ _) = fun (fromRational (fromQ q))
applyDouble fun (R r) = fun r