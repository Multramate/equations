module Simplify where

import Control.Monad (liftM2)
import Data.List (delete, insert)
import Data.Tuple.Extra (both)

import Equation
import Rewrite

--------------------------------------------------------------------------------

-- Simplifies an expression to its simplest form
simplify :: Expression -> Maybe Expression
simplify exp
  | return exp == exp' = exp' >>= rewrite
  | otherwise = exp' >>= simplify
  where
    exp' = fmap reduce (rewrite exp)

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
  Just (Bin opr (Val val) (Val val')) -> exp'''
    where
      exp''' = return $ Val (getBinaryOperation opr val val')
  Just (Bin opr exp''' exp'''') -> exp''
  _ -> Nothing
  where
    exp'' = liftM2 (Bin opr) (evaluate exp) (evaluate exp')
evaluate (App fun exps) = case exp of
  Just (App fun exps') -> if all isValue exps' then exp' else exp
    where
      exp' = fmap (Val . getFunctionApplication fun) (mapM getNumeric exps')
  _ -> Nothing
  where
    exp = fmap (App fun) (mapM evaluate exps)
evaluate exp = return exp

--------------------------------------------------------------------------------

-- Reduces expressions with associative commutative (listable) operators
reduce :: Expression -> Expression
reduce exp
  | exp == exp' = exp'
  | otherwise = reduce exp'
  where
    exp' = convertList exp

-- Converts expressions with listable operators into a function list expression
convertList :: Expression -> Expression
convertList exp''' @ (Bin opr exp (Bin opr' exp' exp''))
  | areListable opr opr' = appendExps (getListFunction opr) exp exp' exp''
convertList exp''' @ (Bin opr (Bin opr' exp exp') exp'')
  | areListable opr opr' = appendExps (getListFunction opr) exp exp' exp''
convertList exp'' @ (Bin opr exp exp')
  | areListable opr opr = appendExps' (getListFunction opr) exp exp'
  | exp'' /= exp''' = convertList exp'''
  where
    exp''' = Bin opr (convertList exp) (convertList exp')
convertList (App fun exps) = App fun (map convertList exps)
convertList exp = exp

-- Appends three expressions into a function list expression
appendExps :: Function -> Expression -> Expression -> Expression -> Expression
appendExps fun exp exp' exp'' = convertList exp'''
  where
    App _ list = foldExp fun [] (convertList exp)
    App _ list' = foldExp fun list (convertList exp')
    exp''' = foldExp fun list' (convertList exp'')

-- Appends two expressions into a function list expression
appendExps' :: Function -> Expression -> Expression -> Expression
appendExps' fun exp exp' = convertList exp''
  where
    App _ list = foldExp fun [] (convertList exp)
    exp'' = foldExp fun list (convertList exp')

-- Folds an expression into a function list expression
foldExp :: Function -> [Expression] -> Expression -> Expression
foldExp fun exps (Bin opr exp exp')
  | fun == Sum && opr == Add || fun == Prd && opr == Mul = App fun exps'
  where
    exps' = foldr (insertTerm opr) exps [exp, exp']
foldExp fun exps (App fun' exps')
  | fun == Sum && fun' == Sum || fun == Prd && fun' == Prd = App fun exps''
  | (fun == Sum || fun == Prd) && fun' == Neg = foldExp fun exps exp
  where
    exps'' = foldr (insertTerm (getOperator fun)) exps exps'
    exp = App Prd (insertTerm Mul (-1) exps')
foldExp fun exps exp = App fun (insertTerm (getOperator fun) exp exps)

-- Inserts a combined term expression into a function list
insertTerm :: Operator -> Expression -> [Expression] -> [Expression]
insertTerm opr exp exps' @ (exp' : exps) = case combineTerms opr exp exp' of
  Just exp'' -> insertTerm opr exp'' exps
  _ -> if exp <= exp' then exp : exps' else exp' : insertTerm opr exp exps
insertTerm _ exp _ = [exp]

-- Combines like term expressions into a combined term expression
combineTerms :: Operator -> Expression -> Expression -> Maybe Expression
combineTerms opr (Val val) (Val val') = return $ Val val''
  where
    val'' = getBinaryOperation opr val val'
combineTerms Add (App Prd exps) (App Prd exps') = case exps'' of
  [] -> Nothing
  _ -> (combineTerms Add exp exp') >>= flip (combineTerms Mul) (App Prd exps'')
  where
    (exps'', (exp, exp')) = fmap (both (App Prd)) (factorTerms exps exps')
combineTerms Add exp exp' @ (App Prd _) = combineTerms Add (App Prd [exp]) exp'
combineTerms Add exp @ (App Prd _) exp' = combineTerms Add exp (App Prd [exp'])
combineTerms _ _ _ = Nothing

-- Factors like terms in an expression list into an expression tuple
factorTerms :: [Expression] -> [Expression]
  -> ([Expression], ([Expression], [Expression]))
factorTerms exps exps' = foldr findTerm ([], ([], exps')) exps
  where
    findTerm exp (exp', (exps, exps'))
      | elem exp exps' = (exp : exp', (exps, delete exp exps'))
      | otherwise = (exp', (exp : exps, exps'))

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

-- Gets the operator of a list function
getOperator :: Function -> Operator
getOperator fun = if fun == Prd then Mul else Add