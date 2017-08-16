module Parse where

import Control.Monad (liftM2)
import Data.Tuple.Extra (both)

import Equation

--------------------------------------------------------------------------------

-- Parses a tuple of token lists with an environment into an equation
parseEqn :: Environment -> (Tokens, Tokens) -> Either String Equation
parseEqn = (.) (uncurry (liftM2 Eqn)) . both . parseExp

-- Parses a token list with an environment into an expression AST
parseExp :: Environment -> Tokens -> Either String Expression
parseExp env toks = convertRPN toks >>= convertAST env

-- Converts a token list into a RPN token queue
convertRPN :: Tokens -> Either String TokenQueue
convertRPN = shuntingYard [] []

-- Converts a RPN token queue with an environment into an expression AST
convertAST :: Environment -> TokenQueue -> Either String Expression
convertAST env queue = case postfixTree env queue of
  Right (exp, []) -> return exp
  Left err -> Left err
  _ -> Left "Parse error: non empty stack"

--------------------------------------------------------------------------------

-- Run the Shunting Yard algorithm on a tokens list
shuntingYard :: TokenQueue -> TokenStack -> Tokens -> Either String TokenQueue
shuntingYard queue stack toks' @ (tok : toks) = case tok of
  Opr opr -> case stack of
    tok' @ (Opr opr') : stack
      | notPrecedes opr opr' -> shuntingYard (tok' : queue) stack toks'
    _ -> shuntingYard queue (tok : stack) toks
  Fun _ -> shuntingYard queue (tok : stack) toks
  Sep -> case stack of
    Opn : stack' -> shuntingYard queue stack toks
    tok' : stack' -> shuntingYard (tok' : queue) stack' toks'
    _ -> Left "Parse error: misplaced separator or missing open bracket"
  Opn -> shuntingYard queue (Opn : stack) toks
  Cls -> case stack of
    Opn : stack' -> case stack' of
      tok' @ (Fun _) : stack'' -> shuntingYard (tok' : queue) stack'' toks
      _ -> shuntingYard queue stack' toks
    tok' : stack' -> shuntingYard (tok' : queue) stack' toks'
    _ -> Left "Parse error: missing open bracket"
  _ -> shuntingYard (tok : queue) stack toks
shuntingYard queue stack _ = case stack of
  Opn : stack' -> Left "Parse error: missing close bracket"
  tok : stack' -> shuntingYard (tok : queue) stack' []
  _ -> return queue

-- Checks if an operator does not precede another operator
notPrecedes :: Operator -> Operator -> Bool
notPrecedes opr opr' = less < more || less == more && left
  where
    less = getPrecedence opr
    more = getPrecedence opr'
    left = getAssociativity opr

--------------------------------------------------------------------------------

-- Runs the postfix tree conversion algorithm on a token queue recursively
postfixTree :: Environment -> TokenQueue
  -> Either String (Expression, TokenQueue)
postfixTree _ (Num num : queue) = return (Val num, queue)
postfixTree env (Chr chr : queue)
  | elem chr env = return (Var chr, queue)
  | otherwise = return (Con chr, queue)
postfixTree env (Opr opr : queue) = case popArguments 2 env queue of
  Right ([exp, exp'], queue') -> return (Bin opr exp' exp, queue')
  Left err -> Left err
  _ -> Left $ "Parse error: miscounted arguments for " ++ show opr
postfixTree env (Fun fun : queue) = case args of
  Right (exps, queue') -> return (App fun (reverse exps), queue')
  Left err -> Left err
  where
    args = popArguments (getArity fun) env queue
postfixTree _ _ = Left "Parse error: miscounted tokens"

-- Pops an arity number of arguments from a token queue recursively
popArguments :: Arity -> Environment -> TokenQueue
  -> Either String (ExpressionStack, TokenQueue)
popArguments 0 _ queue = return ([], queue)
popArguments arity env queue = case postfixTree env queue of
  Right (exp, queue') -> case popArguments (pred arity) env queue' of
    Right (exps, queue'') -> return (exp : exps, queue'')
    Left err -> Left err
  Left err -> Left err