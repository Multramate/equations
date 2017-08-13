module Simplify where

import Control.Monad (liftM2)
import Data.List (insert, sort)

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

-- Applies a list function to associative and commutative operators
applyList :: Expression -> Expression
applyList exp''' @ (Bin opr exp (Bin opr' exp' exp''))
  | opr == opr' = case listFunction opr of
  Just fun -> applyList . App fun . append . append' . append'' $ []
    where
      append = appendList list . App fun
      append' = appendList list' . App fun
      append'' = appendList list'' . App fun
  _ -> if exp''' == exp'''' then exp''' else applyList exp''''
  where
    exp'''' = Bin opr list (Bin opr' list' list'')
    list = applyList exp
    list' = applyList exp'
    list'' = applyList exp''
applyList exp''' @ (Bin opr (Bin opr' exp exp') exp'')
  | opr == opr' = case listFunction opr of
  Just fun -> applyList . App fun . append . append' . append'' $ []
    where
      append = appendList list . App fun
      append' = appendList list' . App fun
      append'' = appendList list'' . App fun
  _ -> if exp''' == exp'''' then exp''' else applyList exp''''
  where
    exp'''' = Bin opr (Bin opr' list list') list''
    list = applyList exp
    list' = applyList exp'
    list'' = applyList exp''
applyList exp'' @ (Bin opr exp exp')
  | exp'' /= exp''' = applyList exp'''
  where
    exp''' = Bin opr (applyList exp) (applyList exp') 
applyList exp = exp

-- Appends two expressions into an expression list
appendList :: Expression -> Expression -> [Expression]
appendList (Bin opr exp exp') (Bin opr' exp'' exp''')
  | opr == Add && opr' == Add || opr == Mul && opr' == Mul
    = sort [exp, exp', exp'', exp''']
appendList (Bin opr exp exp') (App fun exps)
  | opr == Add && fun == Sum || opr == Mul && fun == Prd
    = foldr insert exps [exp, exp']
appendList (App fun exps) (Bin opr exp exp')
  | fun == Sum && opr == Add || fun == Prd && opr == Mul
    = foldr insert exps [exp, exp']
appendList (App fun exps) (App fun' exps')
  | fun == Sum && fun' == Sum || fun == Prd && fun' == Prd
    = foldr insert exps exps'
appendList exp (Bin opr exp' exp'')
  | opr == Add || opr == Mul = sort [exp, exp', exp'']
appendList exp (App fun exps)
  | fun == Sum || fun == Prd = insert exp exps
appendList (Bin opr exp exp') exp''
  | opr == Add || opr == Mul = sort [exp, exp', exp'']
appendList (App fun exps) exp
  | fun == Sum || fun == Prd = insert exp exps
appendList exp exp' = sort [exp, exp']

--------------------------------------------------------------------------------

-- Checks if an expression is a value
isValue :: Expression -> Bool
isValue (Val _) = True
isValue _ = False

-- Gets the numeric of a value expression
getNumeric :: Expression -> Maybe Numeric
getNumeric (Val val) = return val
getNumeric _ = Nothing

-- Returns a list function if an operator is listable
listFunction :: Operator -> Maybe Function
listFunction Add = return Sum
listFunction Mul = return Prd
listFunction _ = Nothing