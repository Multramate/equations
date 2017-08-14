module Simplify where

import Control.Monad (liftM2)
import Data.List (insert)

import Equation
import Rewrite

--------------------------------------------------------------------------------

-- Simplifies an expression to its simplest form
simplify :: Expression -> Maybe Expression
simplify exp
  | return exp == exp' = exp'
  | otherwise = exp' >>= simplify
  where
    exp' = return exp >>= rewrite

--------------------------------------------------------------------------------

-- Rewrites an expression by applying rewriting rules
rewrite :: Expression -> Maybe Expression
rewrite (Bin opr exp exp') = exp'' >>= getRewriteOperation opr >>= evaluate
  where
    exp'' = liftM2 (Bin opr) (rewrite exp) (rewrite exp')
rewrite (App fun exps) = exp >>= getRewriteFunction fun >>= evaluate
  where
    exp = fmap (App fun) (mapM rewrite exps)
rewrite exp = return exp

-- Evaluates an expression by applying operations and functions on values
evaluate :: Expression -> Maybe Expression
evaluate (Bin opr exp exp') = case exp'' of
  Just (Bin opr exp''' exp'''')
    | isValue exp''' && isValue exp'''' -> val''
    | otherwise -> exp''
    where
      val'' = fmap Val (liftM2 (getBinaryOperation opr) val val')
      (val, val') = (getNumeric exp''', getNumeric exp'''')
  _ -> Nothing
  where
    exp'' = liftM2 (Bin opr) (evaluate exp) (evaluate exp')
evaluate (App fun exps) = case exp of
  Just (App fun exps')
    | all isValue exps' -> exp'
    | otherwise -> exp
    where
      exp' = fmap (Val . getFunctionApplication fun) (mapM getNumeric exps')
  _ -> Nothing
  where
    exp = fmap (App fun) (mapM evaluate exps)
evaluate exp = return exp

--------------------------------------------------------------------------------

-- Converts expressions with listable operators into a function list
convertList :: Expression -> Expression
convertList exp''' @ (Bin opr exp (Bin opr' exp' exp''))
  | areListable opr opr' = appendList (getListFunction opr) exp exp' exp''
convertList exp''' @ (Bin opr (Bin opr' exp exp') exp'')
  | areListable opr opr' = appendList (getListFunction opr) exp exp' exp''
convertList exp'' @ (Bin opr exp exp')
  | exp'' /= exp''' = convertList exp'''
  where
    exp''' = Bin opr (convertList exp) (convertList exp')
convertList exp = exp

-- Appends three expressions into a function list
appendList :: Function -> Expression -> Expression -> Expression -> Expression
appendList fun exp exp' exp'' = convertList exp'''
  where
    App _ list = insertList fun [] (convertList exp)
    App _ list' = insertList fun list (convertList exp')
    exp''' = insertList fun list' (convertList exp'')

-- Inserts an expression into a function list
insertList :: Function -> [Expression] -> Expression -> Expression
insertList Sum exps (Bin Add exp exp') = App Sum (foldr insert exps [exp, exp'])
insertList Prd exps (Bin Mul exp exp') = App Prd (foldr insert exps [exp, exp'])
insertList Sum exps (App Sum exps') = App Sum (foldr insert exps exps')
insertList Prd exps (App Prd exps') = App Prd (foldr insert exps exps')
insertList fun exps exp = App fun (insert exp exps)

--------------------------------------------------------------------------------

-- Checks if an expression is a value
isValue :: Expression -> Bool
isValue (Val _) = True
isValue _ = False

-- Gets the numeric of a value expression
getNumeric :: Expression -> Maybe Numeric
getNumeric (Val val) = return val
getNumeric _ = Nothing

-- Checks if two operators are equal and listable
areListable :: Operator -> Operator -> Bool
areListable opr opr' = opr == Add && opr' == Add || opr == Mul && opr' == Mul

-- Gets the list function of an operator
getListFunction :: Operator -> Function
getListFunction opr = if opr == Mul then Prd else Sum