module Lexer where

import Control.Applicative
import Data.Char
import Test.HUnit

import Equation

--------------------------------------------------------------------------------

lexEqn :: String -> Maybe Equation
lexEqn string = case lexExp string of
  (tokens @ (_ : _), '=' : string') -> case lexExp string' of
    (tokens' @ (_ : _), "") -> Just (Eqn tokens tokens')
    _ -> Nothing
  _ -> Nothing

lexExp :: String -> ([Token], String)
lexExp string' @ (char : string)
  | char == '=' = ([], string')
  | isSpace char = lexExp string
  | otherwise = case tokenMatch string' of
    Just (token, string'') -> recurseLex token string''
    _ -> recurseLex (Var char) string
lexExp _ = ([], [])

tokenMatch :: String -> Maybe (Token, String)
tokenMatch string = foldl match empty lookupTable
  where
    match matches (token, operator)
      = matches <|> (,) token <$> patternMatch string operator

patternMatch :: String -> String -> Maybe String
patternMatch (char : string) (char' : pattern)
  | char == char' = patternMatch string pattern
patternMatch string [] = Just string
patternMatch _ _ = Nothing

recurseLex :: Token -> String -> ([Token], String)
recurseLex token string = (token : tokens, string')
  where
    (tokens, string') = lexExp string

--------------------------------------------------------------------------------

actual :: String
actual = "f(x) / log3(4.5) = (ax^2 + b*x - (1+i)c) / (6.7 + 8.9i)"

expected :: Equation
expected = Eqn [
  Var 'f', Opn, Var 'x', Cls, Opr Div, Opr Log,
  Var '3', Opn, Var '4', Var '.', Var '5', Cls
  ] [
  Opn, Var 'a', Var 'x', Opr Exp, Var '2', Opr Add, Var 'b', Opr Mul, Var 'x',
  Opr Sub, Opn, Var '1', Opr Add, Var 'i', Cls, Var 'c', Cls, Opr Div, Opn,
  Var '6', Var '.', Var '7', Opr Add, Var '8', Var '.', Var '9', Var 'i', Cls
  ]

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