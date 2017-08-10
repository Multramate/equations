module Solver where

import Control.Monad (liftM2)

import Equation

--------------------------------------------------------------------------------

-- Evaluates an expression to its simplest form
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

-- Checks if an expression is a value
isValue :: Expression -> Bool
isValue (Val _) = True
isValue _ = False

-- Gets the numeric of a value expression
getNumeric :: Expression -> Maybe Numeric
getNumeric (Val val) = Just val
getNumeric _ = Nothing