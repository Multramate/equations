module Parse where

import Control.Monad (liftM2)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (both)

import Equation

--------------------------------------------------------------------------------

-- Parses a tuple of token lists with an environment into an equation
parseEqn :: Environment -> (Tokens, Tokens) -> Maybe Equation
parseEqn = (.) (uncurry (liftM2 Eqn)) . both . parseExp

-- Parses a token list with an environment into an expression AST
parseExp :: Environment -> Tokens -> Maybe Expression
parseExp env toks = convertRPN toks >>= convertAST env

-- Converts a token list into a RPN token queue
convertRPN :: Tokens -> Maybe TokenQueue
convertRPN = shuntingYard [] []

-- Converts a RPN token queue with an environment into an expression AST
convertAST :: Environment -> TokenQueue -> Maybe Expression
convertAST env queue = case postfixTree env queue of
  Just (exp, []) -> return exp
  _ -> Nothing

--------------------------------------------------------------------------------

-- Run the Shunting Yard algorithm on a tokens list
shuntingYard :: TokenQueue -> TokenStack -> Tokens -> Maybe TokenQueue
shuntingYard queue stack toks' @ (tok : toks) = case tok of
  Opr opr -> case stack of
    tok' @ (Opr opr') : stack
      | notPrecedes opr opr' -> shuntingYard (tok' : queue) stack toks'
    _ -> shuntingYard queue (tok : stack) toks
  Fun _ -> shuntingYard queue (tok : stack) toks
  Sep -> case stack of
    Opn : stack' -> shuntingYard queue stack toks
    tok' : stack' -> shuntingYard (tok' : queue) stack' toks'
    _ -> Nothing
  Opn -> shuntingYard queue (Opn : stack) toks
  Cls -> case stack of
    Opn : stack' -> case stack' of
      tok' @ (Fun _) : stack'' -> shuntingYard (tok' : queue) stack'' toks
      _ -> shuntingYard queue stack' toks
    tok' : stack' -> shuntingYard (tok' : queue) stack' toks'
    _ -> Nothing
  _ -> shuntingYard (tok : queue) stack toks
shuntingYard queue stack _ = case stack of
  Opn : stack' -> Nothing
  tok : stack' -> shuntingYard (tok : queue) stack' []
  _ -> return queue

-- Checks if an operator does not precede another operator
notPrecedes :: Operator -> Operator -> Bool
notPrecedes opr opr' = precedence < precedence' || precedence == precedence'
  && (associativity == LeftAssoc || associativity == BothAssoc)
  where
    precedence = getPrecedence opr
    precedence' = getPrecedence opr'
    associativity = getAssociativity opr

--------------------------------------------------------------------------------

-- Runs the postfix tree conversion algorithm on a token queue recursively
postfixTree :: Environment -> TokenQueue -> Maybe (Expression, TokenQueue)
postfixTree _ (Num num : queue)
  = return (Val num, queue)
postfixTree env (Chr chr : queue)
  = return ((if elem chr env then Var else Con) chr, queue)
postfixTree env (Opr opr : queue)
  = case popArguments 2 env queue of
  Just ([exp, exp'], queue') -> return (Bin opr exp' exp, queue')
  _ -> Nothing
postfixTree env (Fun fun : queue)
  = case popArguments (getArity fun) env queue of
  Just (exps, queue') -> return (App fun (reverse exps), queue')
  _ -> Nothing
postfixTree _ _
  = Nothing

-- Pops an arity number of arguments from a token queue recursively
popArguments :: Arity -> Environment -> TokenQueue
  -> Maybe (ExpressionStack, TokenQueue)
popArguments 0 _ queue = return ([], queue)
popArguments arity env queue = case postfixTree env queue of
  Just (exp, queue') -> case popArguments (pred arity) env queue' of
    Just (exps, queue'') -> return (exp : exps, queue'')
    _ -> Nothing
  _ -> Nothing