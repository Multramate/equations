module Solver where

import Equation

--------------------------------------------------------------------------------

-- Solves a linear equation
solve :: Equation -> Maybe Equation
solve (Eqn exp exp') = Eqn <$> simplify exp <*> simplify exp'

--------------------------------------------------------------------------------

-- Simplifies an expression
simplify :: Expression -> Maybe Expression
simplify (Bin Add exp exp')
  | exp == Val (Z 0) = simplify exp'
  | exp' == Val (Z 0) = simplify exp
simplify (Bin Sub exp exp')
  | exp == Val (Z 0) = fmap negate (simplify exp')
  | exp' == Val (Z 0) = simplify exp
simplify (Bin Mul exp exp')
  | exp == Val (Z 0) || exp' == Val (Z 0) = Just (Val (Z 0))
  | exp == Val (Z 1) = simplify exp'
  | exp' == Val (Z 1) = simplify exp
simplify (Bin Div exp exp')
  | exp' == Val (Z 0) = Nothing
  | exp == Val (Z 0) = Just (Val (Z 0))
  | exp == Val (Z 1) = fmap recip (simplify exp')
  | exp' == Val (Z 1) = simplify exp
simplify (Bin Exp exp exp')
  | exp == Val (Z 0) && exp' == Val (Z 0) = Nothing
  | exp == Val (Z 1) = Just (Val (Z 1))
  | exp == Val (Z 0) = Just (Val (Z 0))
  | exp' == Val (Z 1) = simplify exp
  | exp' == Val (Z 0) = Just (Val (Z 1))
simplify (Bin Jux exp exp')
  | exp == Val (Z 0) || exp' == Val (Z 0) = Just (Val (Z 0))
  | exp == Val (Z 1) = simplify exp'
  | exp' == Val (Z 1) = simplify exp
simplify (Bin opr exp exp') = case Bin opr <$> simplify exp <*> simplify exp' of
  Just (Bin opr (Val val) (Val val')) -> Just (Val val'')
    where
      val'' = getBinaryOperation opr val val'
  Just exp -> Just exp
  _ -> Nothing
simplify exp = Just exp