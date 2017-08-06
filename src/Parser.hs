module Parser where

import Data.Maybe

import Equation

--------------------------------------------------------------------------------

convertRPN :: Tokens -> Maybe Queue
convertRPN = fmap reverse . shuntingYard [] []

shuntingYard :: Queue -> Stack -> Tokens -> Maybe Queue
shuntingYard queue stack tokens' @ (token : tokens) = case token of
  Con _ -> shuntingYard (token : queue) stack tokens
  Var _ -> shuntingYard (token : queue) stack tokens
  Opr opr -> case stack of
    token' @ (Opr opr') : stack
      | notPrecedes opr opr' -> shuntingYard (token' : queue) stack tokens'
    _ -> shuntingYard queue (token : stack) tokens
  Opn -> shuntingYard queue (Opn : stack) tokens
  Cls -> case stack of
    Opn : stack' -> shuntingYard queue stack' tokens
    token' : stack' -> shuntingYard (token' : queue) stack' tokens'
    _ -> Nothing
  _ -> Nothing
shuntingYard queue stack _ = case stack of
  Opn : stack' -> Nothing
  token : stack' -> shuntingYard (token : queue) stack' []
  _ -> Just queue

notPrecedes :: Operator -> Operator -> Bool
notPrecedes opr opr' = less < more || less == more && left
  where
    (less, left) = fromJust (lookup opr precedenceTable)
    (more, _) = fromJust (lookup opr' precedenceTable)