module Parser where

import Data.Maybe (fromJust)

import Equation

--------------------------------------------------------------------------------

convertRPN :: Tokens -> Maybe Queue
convertRPN = fmap reverse . shuntingYard [] []

shuntingYard :: Queue -> Stack -> Tokens -> Maybe Queue
shuntingYard queue stack tokens' @ (token : tokens) = case token of
  Opr opr -> case stack of
    token' @ (Opr opr') : stack
      | notPrecedes opr opr' -> shuntingYard (token' : queue) stack tokens'
    _ -> shuntingYard queue (token : stack) tokens
  Fun _ -> shuntingYard queue (token : stack) tokens
  Sep -> case stack of
    Opn : stack' -> shuntingYard queue stack tokens
    token' : stack' -> shuntingYard (token' : queue) stack' tokens'
    _ -> Nothing
  Opn -> shuntingYard queue (Opn : stack) tokens
  Cls -> case stack of
    Opn : stack' -> case stack' of
      token' @ (Fun _) : stack'' -> shuntingYard (token' : queue) stack'' tokens
      _ -> shuntingYard queue stack' tokens
    token' : stack' -> shuntingYard (token' : queue) stack' tokens'
    _ -> Nothing
  Not -> Nothing
  _ -> shuntingYard (token : queue) stack tokens
shuntingYard queue stack _ = case stack of
  Opn : stack' -> Nothing
  token : stack' -> shuntingYard (token : queue) stack' []
  _ -> Just queue

--------------------------------------------------------------------------------

notPrecedes :: Operator -> Operator -> Bool
notPrecedes opr opr' = less < more || less == more && left
  where
    less = getPrecedence opr
    more = getPrecedence opr'
    left = getAssociativity opr