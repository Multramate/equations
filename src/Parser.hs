module Parser where

import Data.Maybe (fromJust)
import Data.Tuple.Extra (both)

import Equation

--------------------------------------------------------------------------------

parseEqn :: Environment -> (Tokens, Tokens) -> Maybe Equation
parseEqn = (.) (uncurry ((<*>) . (<$>) Eqn)) . both . parseExp

parseExp :: Environment -> Tokens -> Maybe Expression
parseExp env tokens = convertRPN tokens >>= convertAST env

convertRPN :: Tokens -> Maybe TokenQueue
convertRPN = shuntingYard [] []

convertAST :: Environment -> TokenQueue -> Maybe Expression
convertAST env queue = case postfixTree env queue of
  Just (exp, []) -> Just exp
  _ -> Nothing

--------------------------------------------------------------------------------

shuntingYard :: TokenQueue -> TokenStack -> Tokens -> Maybe TokenQueue
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
  _ -> shuntingYard (token : queue) stack tokens
shuntingYard queue stack _ = case stack of
  Opn : stack' -> Nothing
  token : stack' -> shuntingYard (token : queue) stack' []
  _ -> Just queue

notPrecedes :: Operator -> Operator -> Bool
notPrecedes opr opr' = less < more || less == more && left
  where
    less = getPrecedence opr
    more = getPrecedence opr'
    left = getAssociativity opr

--------------------------------------------------------------------------------

postfixTree :: Environment -> TokenQueue -> Maybe (Expression, TokenQueue)
postfixTree _ (Num num : queue)
  = Just (Val num, queue)
postfixTree env (Chr chr : queue)
  = Just ((if elem chr env then Var else Con) chr, queue)
postfixTree env (Opr opr : queue)
  = case popArguments 2 env queue of
  Just ([arg, arg'], queue') -> Just (Bin opr arg' arg, queue')
  Nothing -> Nothing
postfixTree env (Fun fun : queue)
  = case popArguments (getArity fun) env queue of
  Just (args, queue') -> Just (App fun (reverse args), queue')
  Nothing -> Nothing
postfixTree _ _ = Nothing

popArguments :: Arity -> Environment -> TokenQueue
  -> Maybe (ExpressionStack, TokenQueue)
popArguments 0 _ queue = Just ([], queue)
popArguments arity env queue = case postfixTree env queue of
  Just (arg, queue') -> case popArguments (pred arity) env queue' of
    Just (args, queue'') -> Just (arg : args, queue'')
    _ -> Nothing
  _ -> Nothing