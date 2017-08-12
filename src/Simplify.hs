module Simplify where

import Control.Monad (liftM2)

import Equation
import Rewrite

--------------------------------------------------------------------------------

-- Simplifies an expression to its simplest form
simplify :: Expression -> Maybe Expression
simplify exp
  | return exp == exp' = exp'
  | otherwise = exp' >>= simplify
  where
    exp' = rewrite exp >>= reassociate

--------------------------------------------------------------------------------

-- Reassociates an expression by rearranging brackets and rewriting
reassociate :: Expression -> Maybe Expression
reassociate = (=<<) reassociateLeft . (=<<) reassociateRight . return
  where
    reassociateLeft = (=<<) rewrite . bracketLeft
    reassociateRight = (=<<) rewrite . bracketRight

-- Brackets associative operators to the left
bracketLeft :: Expression -> Maybe Expression
bracketLeft (Bin opr exp (Bin opr' exp' exp''))
  | isAssoc = exp'''' >>= bracketLeft
  where
    isAssoc = opr == opr' && getAssociativity opr == BothAssoc
    exp'''' = liftM2 (Bin opr) (exp''' >>= bracketLeft) (bracketLeft exp'')
    exp''' = liftM2 (Bin opr') (bracketLeft exp) (bracketLeft exp')
bracketLeft (Bin opr exp exp')
  = liftM2 (Bin opr) (bracketLeft exp) (bracketLeft exp')
bracketLeft exp = return exp

-- Brackets associative operators to the right
bracketRight :: Expression -> Maybe Expression
bracketRight (Bin opr (Bin opr' exp exp') exp'')
  | isAssoc = exp'''' >>= bracketRight
  where
    isAssoc = opr == opr' && getAssociativity opr == BothAssoc
    exp'''' = liftM2 (Bin opr) (bracketRight exp) (exp''' >>= bracketRight)
    exp''' = liftM2 (Bin opr') (bracketRight exp') (bracketRight exp'')
bracketRight (Bin opr exp exp') = liftM2 (Bin opr) exp'' exp'''
  where
    exp'' = bracketRight exp
    exp''' = bracketRight exp'
bracketRight exp = return exp

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

-- Checks if an expression is a value
isValue :: Expression -> Bool
isValue (Val _) = True
isValue _ = False

-- Gets the numeric of a value expression
getNumeric :: Expression -> Maybe Numeric
getNumeric (Val val) = return val
getNumeric _ = Nothing