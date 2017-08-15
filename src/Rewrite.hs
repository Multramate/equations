module Rewrite where

import Data.Maybe (fromJust)

import Equation

--------------------------------------------------------------------------------

-- Gets the rewrite of a binary operation
getRewriteOperation :: Operator -> Expression -> Maybe Expression
getRewriteOperation = (.) fromJust . flip lookup $
  [ (Add, rewriteAdd)
  , (Sub, rewriteSub)
  , (Mul, rewriteMul)
  , (Div, rewriteDiv)
  , (Exp, rewriteExp)
  , (Jux, rewriteMul) ]

-- Gets the rewrite of a function application
getRewriteFunction :: Function -> Expression -> Maybe Expression
getRewriteFunction = (.) fromJust . flip lookup $
  [ (Sum, rewriteSum)
  , (Prd, rewritePrd)
  , (Log, rewriteLog)
  , (Neg, rewriteNeg)
  , (Rcp, rewriteRcp) ]

--------------------------------------------------------------------------------

-- Rewrites an addition operation
rewriteAdd :: Expression -> Maybe Expression
rewriteAdd (Bin _ exp exp')
  | exp == negate exp' = return 0     -- -x + x = 0
  | exp' == negate exp = return 0     -- x + -x = 0
rewriteAdd (Bin _ 0 exp) = return exp -- 0 + x = x
rewriteAdd (Bin _ exp 0) = return exp -- x + 0 = x
rewriteAdd exp = return exp

-- Rewrites a subtraction operation
rewriteSub :: Expression -> Maybe Expression
rewriteSub (Bin _ exp exp')
  | exp == exp' = return 0                     -- x - x = 0
rewriteSub (Bin _ exp 0) = return exp          -- x - 0 = x
rewriteSub (Bin _ 0 exp) = return $ negate exp -- 0 - x = -x
rewriteSub (Bin _ exp exp') = return $ Bin Add exp (negate exp')
rewriteSub exp = return exp

-- Rewrites a multiplication operation
rewriteMul :: Expression -> Maybe Expression
rewriteMul (Bin _ 0 _) = return 0                  -- 0 * x = 0
rewriteMul (Bin _ _ 0) = return 0                  -- x * 0 = 0
rewriteMul (Bin _ exp exp')
  | exp == recip exp' = return 1                   -- 1/x * x = 1
  | exp' == recip exp = return 1                   -- x * 1/x = 1
  | exp == negate (recip exp') = return $ Val (-1) -- -1/x * x = -1
  | exp == recip (negate exp') = return $ Val (-1) -- 1/-x * x = -1
  | exp' == negate (recip exp) = return $ Val (-1) -- x * -1/x = -1
  | exp' == recip (negate exp) = return $ Val (-1) -- x * 1/-x = -1
rewriteMul (Bin _ 1 exp) = return exp              -- 1 * x = x
rewriteMul (Bin _ exp 1) = return exp              -- x * 1 = x
rewriteMul (Bin _ exp exp')
  | exp == Val (-1) = return $ negate exp'         -- -1 * x = -x
  | exp' == Val (-1) = return $ negate exp         -- x * -1 = -x
rewriteMul exp = return exp

-- Rewrites a division operation
rewriteDiv :: Expression -> Maybe Expression
rewriteDiv (Bin _ _ 0) = Nothing                   -- division by zero
rewriteDiv (Bin _ 0 exp) = return 0                -- 0 / x = 0
rewriteDiv (Bin _ exp exp')
  | exp == exp' = return 1                         -- x / x = 1
  | exp == negate exp' = return $ Val (-1)         -- -x / x = -1
  | exp' == negate exp = return $ Val (-1)         -- x / -x = -1
rewriteDiv (Bin _ exp 1) = return exp              -- x / 1 = x
rewriteDiv (Bin _ 1 exp) = return $ recip exp      -- 1 / x = 1/x
rewriteDiv (Bin _ exp exp')
  | exp' == Val (-1) = return $ negate exp         -- x / -1 = -x
  | exp == Val (-1) = return $ negate (recip exp') -- -1 / x = -1/x
rewriteDiv (Bin _ exp exp') = return $ Bin Mul exp (recip exp')
rewriteDiv exp = return exp

-- Rewrites an exponentiation operation
rewriteExp :: Expression -> Maybe Expression
rewriteExp (Bin _ 0 0) = Nothing          -- zero power zero
rewriteExp (Bin _ exp exp')
  | exp /= 0 && exp <= 0 = Nothing        -- negative exponent base
rewriteExp (Bin _ 0 _) = return 0         -- 0 ^ x = 0
rewriteExp (Bin _ _ 0) = return 1         -- x ^ 0 = 1
rewriteExp (Bin _ 1 _) = return 1         -- 1 ^ x = 1
rewriteExp (Bin _ exp 1) = return exp     -- x ^ 1 = x
rewriteExp (Bin _ exp exp')
  | exp' == Val (-1) = return $ recip exp -- x ^ -1 = 1/x
rewriteExp exp = return exp

--------------------------------------------------------------------------------

-- Rewrites a sum function
rewriteSum :: Expression -> Maybe Expression
rewriteSum (App _ []) = return 0
rewriteSum (App _ [exp]) = return exp
rewriteSum (App _ [exp, exp']) = return $ Bin Add exp exp'
rewriteSum exp = return exp

-- Rewrites a product function
rewritePrd :: Expression -> Maybe Expression
rewritePrd (App _ []) = return 1
rewritePrd (App _ [exp]) = return exp
rewritePrd (App _ [exp, exp']) = return $ Bin Mul exp exp'
rewritePrd exp = return exp

-- Rewrites a logarithm function
rewriteLog :: Expression -> Maybe Expression
rewriteLog (App _ [1, _]) = Nothing  -- division by zero
rewriteLog (App _ [exp, exp'])
  | exp <= 0 = Nothing               -- log of negative
  | exp' <= 0 = Nothing              -- log of negative
rewriteLog (App _ [_, 1]) = return 0 -- log [x, 1] = 0
rewriteLog (App _ [exp, exp'])
  | exp == exp' = return 1           -- log [x, x] = 1
rewriteLog exp = return exp

-- Rewrites a negate function
rewriteNeg :: Expression -> Maybe Expression
rewriteNeg (App _ [App Neg [exp]]) = return exp -- neg [neg [x]] = x
rewriteNeg exp = return exp

-- Rewrites a reciprocal function
rewriteRcp :: Expression -> Maybe Expression
rewriteRcp (App _ [App Rcp [exp]]) = return exp -- rcp [rcp [x]] = x
rewriteRcp (App _ [App Neg [exp]])
  = return $ App Neg [App Rcp [exp]]            -- rcp [neg [x]] = neg [rcp [x]]
rewriteRcp exp = return exp