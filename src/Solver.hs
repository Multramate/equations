module Solver where

import Control.Monad (liftM2)
import Data.Maybe (fromJust, isJust, isNothing)

import Equation

--------------------------------------------------------------------------------

-- Evaluates an expression by applying operations and functions on values
evaluate :: Expression -> Maybe Expression
evaluate (Bin opr exp exp') = case exp'' of
  Just (Bin Div _ 0) -> Nothing
  Just (Bin Exp 0 0) -> Nothing
  Just (Bin opr (Val val) (Val val')) -> Just (Val val'')
    where
      val'' = getBinaryOperation opr val val'
  _ -> exp''
  where
    exp'' = liftM2 (Bin opr) (evaluate exp) (evaluate exp')
evaluate (App fun exps) = case exp of
  Just (App fun exps') -> if all isValue exps' then exp' else exp
    where
      exp' = fmap (Val . getFunctionApplication fun) (mapM getNumeric exps')
  _ -> Nothing
  where
    exp = fmap (App fun) (mapM evaluate exps)
evaluate exp = Just exp

--------------------------------------------------------------------------------

-- Rewrites an expression by applying rewriting rules
rewrite :: Expression -> Maybe Expression
rewrite (Bin opr exp exp') = exp'' >>= getRewriteOperation opr >>= evaluate 
  where
    exp'' = liftM2 (Bin opr) (rewrite exp) (rewrite exp')
rewrite (App fun exps) = exp >>= getRewriteFunction fun >>= evaluate
  where
    exp = fmap (App fun) (mapM rewrite exps)
rewrite exp = Just exp

--------------------------------------------------------------------------------

-- Rewrites an add operation by applying add rewriting rules
rewriteAdd :: Expression -> Maybe Expression
rewriteAdd (Bin _ 0 exp) = Just exp -- 0 + x = x
rewriteAdd (Bin _ exp 0) = Just exp -- x + 0 = x
rewriteAdd (Bin _ exp exp')
  | exp == negate exp' = Just 0     -- -x + x = 0
  | exp' == negate exp = Just 0     -- x + -x = 0
rewriteAdd exp = Just exp

-- Rewrites a sub operation by applying sub rewriting rules
rewriteSub :: Expression -> Maybe Expression
rewriteSub (Bin _ 0 exp) = Just (negate exp) -- 0 - x = -x
rewriteSub (Bin _ exp 0) = Just exp          -- x - 0 = x
rewriteSub (Bin _ exp exp')
  | exp == exp' = Just 0                     -- x - x = 0
rewriteSub exp = Just exp

-- Rewrites a mul or jux operation by applying mul rewriting rules
rewriteMul :: Expression -> Maybe Expression
rewriteMul (Bin _ 0 _) = Just 0              -- 0 * x = 0
rewriteMul (Bin _ _ 0) = Just 0              -- x * 0 = 0
rewriteMul (Bin _ 1 exp) = Just exp          -- 1 * x = x
rewriteMul (Bin _ exp 1) = Just exp          -- x * 1 = x
rewriteMul (Bin _ exp exp')
  | exp == Val (Z (-1)) = Just (negate exp') -- -1 * x = -x
  | exp' == Val (Z (-1)) = Just (negate exp) -- x * -1 = -x
rewriteMul (Bin _ exp exp')
  | exp == recip exp' = Just 1               -- 1/x * x = 1
  | exp' == recip exp = Just 1               -- x * 1/x = 1
rewriteMul exp = Just exp

-- Rewrites a div operation by applying div rewriting rules
rewriteDiv :: Expression -> Maybe Expression
rewriteDiv (Bin _ _ 0) = Nothing                     -- error: division by zero
rewriteDiv (Bin _ 0 exp) = Just 0                    -- 0 / x = 0
rewriteDiv (Bin _ 1 exp) = Just (recip exp)          -- 1 / x = 1/x
rewriteDiv (Bin _ exp 1) = Just exp                  -- x / 1 = x
rewriteDiv (Bin _ exp exp')
  | exp == exp' = Just 1                             -- x / x = 1
  | exp == Val (Z (-1)) = Just (negate (recip exp')) -- -1 / x = -1/x
  | exp' == Val (Z (-1)) = Just (negate exp)         -- x / -1 = -x
rewriteDiv exp = Just exp

-- Rewrites an exp operation by applying exp rewriting rules
rewriteExp :: Expression -> Maybe Expression
rewriteExp (Bin _ 0 0) = Nothing            -- error: zero power zero
rewriteExp (Bin _ exp exp')
  | exp /= 0 && exp <= 0 = Nothing          -- error: negative exponent base
  | exp' == Val (Z (-1)) = Just (recip exp) -- x ^ -1 = 1/x
rewriteExp (Bin _ 0 _) = Just 0             -- 0 ^ x = 0
rewriteExp (Bin _ _ 0) = Just 1             -- x ^ 0 = 1
rewriteExp (Bin _ 1 _) = Just 1             -- 1 ^ x = 1
rewriteExp (Bin _ exp 1) = Just exp         -- x ^ 1 = x
rewriteExp exp = Just exp

--------------------------------------------------------------------------------

-- Rewrites a log function by applying log rewriting rules
rewriteLog :: Expression -> Maybe Expression
rewriteLog (App _ [1, _]) = Nothing -- error: division by zero
rewriteLog (App _ [exp, exp'])
  | exp <= 0 = Nothing              -- error: log of negative
  | exp' <= 0 = Nothing             -- error: log of negative
rewriteLog (App _ [_, 1]) = Just 0  -- log [x, 1] = 0
rewriteLog (App _ [exp, exp'])
  | exp == exp' = Just 1            -- log [x, x] = 1
rewriteLog exp = Just exp

-- Rewrites a neg function by applying neg rewriting rules
rewriteNeg :: Expression -> Maybe Expression
rewriteNeg (App _ [App Neg [exp]]) = Just exp -- neg [neg [x]] = x
rewriteNeg exp = Just exp

--------------------------------------------------------------------------------

-- Checks if an expression is a value
isValue :: Expression -> Bool
isValue (Val _) = True
isValue _ = False

-- Gets the numeric of a value expression
getNumeric :: Expression -> Maybe Numeric
getNumeric (Val val) = Just val
getNumeric _ = Nothing

-- Gets the rewrite of a binary operation
getRewriteOperation :: Operator -> (Expression -> Maybe Expression)
getRewriteOperation = (.) fromJust . flip lookup $
  [ (Add, rewriteAdd)
  , (Sub, rewriteSub)
  , (Mul, rewriteMul)
  , (Div, rewriteDiv)
  , (Exp, rewriteExp)
  , (Jux, rewriteMul) ]

-- Gets the rewrite of a function application
getRewriteFunction :: Function -> (Expression -> Maybe Expression)
getRewriteFunction = (.) fromJust . flip lookup $
  [ (Log, rewriteLog)
  , (Neg, rewriteNeg) ]