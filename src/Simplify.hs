module Simplify where

import Control.Monad (liftM2)
import Data.List (insert)

import Equation
import Reduce

--------------------------------------------------------------------------------

-- Simplifies an expression to its simplest form
simplify :: Expression -> Either String Expression
simplify exp
  | return exp == exp' = exp' >>= reduce
  | otherwise = exp' >>= simplify
  where
    exp' = fmap rewrite (reduce exp)

--------------------------------------------------------------------------------

-- Reduces an expression by applying reduction rules
reduce :: Expression -> Either String Expression
reduce (Bin opr exp exp') = exp'' >>= getReduceOperation opr >>= evaluate
  where
    exp'' = liftM2 (Bin opr) (reduce exp) (reduce exp')
reduce (App fun exps) = exp >>= getReduceFunction fun >>= evaluate
  where
    exp = fmap (App fun) (mapM reduce exps)
reduce exp = return exp

-- Evaluates an expression by applying operations and functions on values
evaluate :: Expression -> Either String Expression
evaluate (Bin opr exp exp') = case exp'' of
  Right (Bin opr (Val val) (Val val')) -> exp'''
    where
      exp''' = return $ Val (getBinaryOperation opr val val')
  Right (Bin opr exp''' exp'''') -> exp''
  Left err -> Left err
  _ -> Left "Reduction error: evaluation error"
  where
    exp'' = liftM2 (Bin opr) (evaluate exp) (evaluate exp')
evaluate (App fun exps) = case exp of
  Right (App fun exps') -> if all isValue exps' then exp' else exp
    where
      exp' = fmap (Val . getFunctionApplication fun) (mapM getNumeric exps')
  Left err -> Left err
  _ -> Left "Reduction error: evaluation error"
  where
    exp = fmap (App fun) (mapM evaluate exps)
    isValue (Val _) = True
    isValue _ = False
    getNumeric (Val val) = return val
evaluate exp = return exp

--------------------------------------------------------------------------------

-- Rewrites an expression with associative commutative (listable) operators
rewrite :: Expression -> Expression
rewrite exp = case exp of
  Bin opr exp' (Bin opr' exp'' exp''')
    | listable opr opr' -> append3 (fun' opr) exp' exp'' exp'''
  Bin opr (Bin opr' exp' exp'') exp'''
    | listable opr opr' -> append3 (fun' opr) exp' exp'' exp'''
  Bin opr exp' exp''
    | listable opr opr -> append2 (fun' opr) exp' exp''
    | exp /= exp''' -> rewrite exp'''
    where
      exp''' = Bin opr (rewrite exp') (rewrite exp'')
  App fun exps -> App fun (map rewrite exps)
  _ -> exp
  where
    listable opr opr' = opr == Add && opr' == Add || opr == Mul && opr' == Mul
    fun' opr = if opr == Mul then Prd else Sum
    append3 fun exp = append2 fun . append2 fun exp
    append2 fun exp = foldExp fun list . rewrite
      where
        App _ list = foldExp fun [] (rewrite exp)

-- Folds an expression into a function list
foldExp :: Function -> [Expression] -> Expression -> Expression
foldExp fun exps exp = case exp of
  Bin opr exp' exp''
    | fun == Sum && opr == Add || fun == Prd && opr == Mul -> App fun exps'
    where
      exps' = foldr insert exps [exp', exp'']
  App fun' exps'
    | fun == Sum && fun' == Sum || fun == Prd && fun' == Prd -> App fun exps''
    where
      exps'' = foldr insert exps exps'
  _ -> App fun (insert exp exps)
  where
    opr' = if fun == Prd then Mul else Add