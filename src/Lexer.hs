module Lexer where

import Control.Applicative
import Data.Char
import Numeric
import Test.HUnit

import Equation
import Rational

--------------------------------------------------------------------------------

lexEqn :: String -> Maybe Equation
lexEqn string = case lexExp string of
  (tokens @ (_ : _), '=' : string') -> case lexExp string' of
    (tokens' @ (_ : _), []) -> Just (Eqn tokens tokens')
    _ -> Nothing
  _ -> Nothing

lexExp :: String -> ([Token], String)
lexExp string @ (char : string')
  | char == '=' = ([], string)
  | char == '(' = recurseLex Opn string'
  | char == ')' = recurseLex Cls string'
  | isSpace char = lexExp string'
  | isDigit char = case constantMatch string of
    Just (constant, string'') -> recurseLex (Con constant) string''
    _ -> recurseLex Not string'
  | otherwise = case operatorMatch string of
    Just (operator, string'') -> recurseLex (Opr operator) string''
    _ -> recurseLex (if isAlpha char then Var char else Not) string'
lexExp _ = ([], [])

recurseLex :: Token -> String -> ([Token], String)
recurseLex token string = (token : tokens, string')
  where
    (tokens, string') = lexExp string

constantMatch :: String -> Maybe (Constant, String)
constantMatch string = case reads string :: [(Int, String)] of
  [(int, string')] -> Just (Z int, string')
  _ -> case readFloat string :: [(Rational, String)] of
    [(rational, string')] -> Just (toQ rational, string')
    _ -> Nothing

operatorMatch :: String -> Maybe (Operator, String)
operatorMatch string = foldl match empty lookupTable
  where
    match matches (token, operator)
      = matches <|> (,) token <$> patternMatch string operator

patternMatch :: String -> String -> Maybe String
patternMatch (char : string) (char' : pattern)
  | char == char' = patternMatch string pattern
patternMatch string [] = Just string
patternMatch _ _ = Nothing

--------------------------------------------------------------------------------

actual :: String
actual = "f(x) / log3(4*5) = (ax^2 - (b*x + c1)) / (6.7 ^ -8.9)"

expected :: Equation
expected = Eqn
  [ Var 'f', Opn, Var 'x', Cls, Opr Div, Opr Log
  , Con (Z 3), Opn, Con (Z 4), Opr Mul, Con (Z 5), Cls ]
  [ Opn, Var 'a', Var 'x', Opr Exp, Con (Z 2), Opr Sub, Opn
  , Var 'b', Opr Mul, Var 'x', Opr Add, Var 'c', Con (Z 1), Cls, Cls
  , Opr Div, Opn, Con (Q 67 10), Opr Exp, Opr Sub, Con (Q 89 10), Cls ]

tests :: Test
tests = TestList
  [ lexEqn "=" ~?= Nothing
  , lexEqn "x=" ~?= Nothing
  , lexEqn "=x" ~?= Nothing
  , lexEqn "==" ~?= Nothing
  , lexEqn "x==" ~?= Nothing
  , lexEqn "=x=" ~?= Nothing
  , lexEqn "==x" ~?= Nothing
  , lexEqn "x=x=" ~?= Nothing
  , lexEqn "x==x" ~?= Nothing
  , lexEqn "=x=x" ~?= Nothing
  , lexEqn "x=x=x" ~?= Nothing
  , lexEqn "===" ~?= Nothing
  , lexEqn "x===" ~?= Nothing
  , lexEqn "=x==" ~?= Nothing
  , lexEqn "==x=" ~?= Nothing
  , lexEqn "===x" ~?= Nothing
  , lexEqn "x=x==" ~?= Nothing
  , lexEqn "x==x=" ~?= Nothing
  , lexEqn "x===x" ~?= Nothing
  , lexEqn "=x=x=" ~?= Nothing
  , lexEqn "=x==x" ~?= Nothing
  , lexEqn "==x=x" ~?= Nothing
  , lexEqn "x=x=x=" ~?= Nothing
  , lexEqn "x=x==x" ~?= Nothing
  , lexEqn "x==x=x" ~?= Nothing
  , lexEqn "=x=x=x" ~?= Nothing
  , lexEqn "x=x=x=x" ~?= Nothing
  , lexEqn actual ~?= Just expected
  ]