module Reduce where

import Data.Maybe (fromJust)

import Equation

--------------------------------------------------------------------------------

-- Gets the reduce of a binary operation
getReduceOperation :: Operator -> Expression -> Either String Expression
getReduceOperation = (.) fromJust . flip lookup $
  [ (Add, reduceAdd)
  , (Sub, reduceSub)
  , (Mul, reduceMul)
  , (Div, reduceDiv)
  , (Exp, reduceExp)
  , (Jux, reduceMul) ]

-- Gets the reduce of a function application
getReduceFunction :: Function -> Expression -> Either String Expression
getReduceFunction = (.) fromJust . flip lookup $
  [ (Sum, reduceSum)
  , (Prd, reducePrd)
  , (Log, reduceLog)
  , (Neg, reduceNeg)
  , (Rcp, reduceRcp) ]

--------------------------------------------------------------------------------

-- Reduces an addition operation
reduceAdd :: Expression -> Either String Expression
reduceAdd (Bin _ exp exp')
  | exp == negate exp' = return 0    -- -x + x = 0
  | exp' == negate exp = return 0    -- x + -x = 0
reduceAdd (Bin _ 0 exp) = return exp -- 0 + x = x
reduceAdd (Bin _ exp 0) = return exp -- x + 0 = x
reduceAdd exp = return exp

-- Reduces a subtraction operation
reduceSub :: Expression -> Either String Expression
reduceSub (Bin _ exp exp')
  | exp == exp' = return 0                    -- x - x = 0
reduceSub (Bin _ exp 0) = return exp          -- x - 0 = x
reduceSub (Bin _ 0 exp) = return $ negate exp -- 0 - x = -x
reduceSub (Bin _ exp exp') = return $ Bin Add exp (negate exp')
reduceSub exp = return exp

-- Reduces a multiplication operation
reduceMul :: Expression -> Either String Expression
reduceMul (Bin _ 0 _) = return 0                   -- 0 * x = 0
reduceMul (Bin _ _ 0) = return 0                   -- x * 0 = 0
reduceMul (Bin _ exp exp')
  | exp == recip exp' = return 1                   -- 1/x * x = 1
  | exp' == recip exp = return 1                   -- x * 1/x = 1
  | exp == negate (recip exp') = return $ Val (-1) -- -1/x * x = -1
  | exp == recip (negate exp') = return $ Val (-1) -- 1/-x * x = -1
  | exp' == negate (recip exp) = return $ Val (-1) -- x * -1/x = -1
  | exp' == recip (negate exp) = return $ Val (-1) -- x * 1/-x = -1
reduceMul (Bin _ 1 exp) = return exp               -- 1 * x = x
reduceMul (Bin _ exp 1) = return exp               -- x * 1 = x
reduceMul (Bin _ exp exp')
  | exp == Val (-1) = return $ negate exp'         -- -1 * x = -x
  | exp' == Val (-1) = return $ negate exp         -- x * -1 = -x
reduceMul exp = return exp

-- Reduces a division operation
reduceDiv :: Expression -> Either String Expression
reduceDiv (Bin _ _ 0) = Left "Reduction error: division by zero"
reduceDiv (Bin _ 0 exp) = return 0                 -- 0 / x = 0
reduceDiv (Bin _ exp exp')
  | exp == exp' = return 1                         -- x / x = 1
  | exp == negate exp' = return $ Val (-1)         -- -x / x = -1
  | exp' == negate exp = return $ Val (-1)         -- x / -x = -1
reduceDiv (Bin _ exp 1) = return exp               -- x / 1 = x
reduceDiv (Bin _ 1 exp) = return $ recip exp       -- 1 / x = 1/x
reduceDiv (Bin _ exp exp')
  | exp' == Val (-1) = return $ negate exp         -- x / -1 = -x
  | exp == Val (-1) = return $ negate (recip exp') -- -1 / x = -1/x
reduceDiv (Bin _ exp exp') = return $ Bin Mul exp (recip exp')
reduceDiv exp = return exp

-- Reduces an exponentiation operation
reduceExp :: Expression -> Either String Expression
reduceExp (Bin _ 0 0) = Left "Reduction error: zero power zero"
reduceExp (Bin _ exp exp')
  | exp /= 0 && exp <= 0 = Left "Reduction error: negative exponent base"
reduceExp (Bin _ 0 _) = return 0          -- 0 ^ x = 0
reduceExp (Bin _ _ 0) = return 1          -- x ^ 0 = 1
reduceExp (Bin _ 1 _) = return 1          -- 1 ^ x = 1
reduceExp (Bin _ exp 1) = return exp      -- x ^ 1 = x
reduceExp (Bin _ exp exp')
  | exp' == Val (-1) = return $ recip exp -- x ^ -1 = 1/x
reduceExp exp = return exp

--------------------------------------------------------------------------------

-- Reduces a sum function
reduceSum :: Expression -> Either String Expression
reduceSum (App _ []) = return 0
reduceSum (App _ [exp]) = return exp
reduceSum exp = return exp

-- Reduces a product function
reducePrd :: Expression -> Either String Expression
reducePrd (App _ []) = return 1
reducePrd (App _ [exp]) = return exp
reducePrd exp = return exp

-- Reduces a logarithm function
reduceLog :: Expression -> Either String Expression
reduceLog (App _ [1, _]) = Left "Reduction error: log base one"
reduceLog (App _ [exp, exp'])
  | exp <= 0 || exp' <= 0 = Left "Reduction error: log of non positive"
reduceLog (App _ [_, 1]) = return 0 -- log [x, 1] = 0
reduceLog (App _ [exp, exp'])
  | exp == exp' = return 1          -- log [x, x] = 1
reduceLog exp = return exp

-- Reduces a negate function
reduceNeg :: Expression -> Either String Expression
reduceNeg (App _ [App Neg [exp]]) = return exp -- neg [neg [x]] = x
reduceNeg exp = return exp

-- Reduces a reciprocal function
reduceRcp :: Expression -> Either String Expression
reduceRcp (App _ [App Rcp [exp]]) = return exp -- rcp [rcp [x]] = x
reduceRcp (App _ [App Neg [exp]])
  = return $ App Neg [App Rcp [exp]]           -- rcp [neg [x]] = neg [rcp [x]]
reduceRcp exp = return exp