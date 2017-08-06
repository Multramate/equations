module Lexer where

import Control.Applicative
import Data.Char
import Numeric

import Equation
import Rational

--------------------------------------------------------------------------------

lexEqn :: Input -> Maybe Equation
lexEqn string = case lexExp string of
  (tokens @ (_ : _), '=' : string') -> case lexExp string' of
    (tokens' @ (_ : _), []) -> Just (Eqn tokens tokens')
    _ -> Nothing
  _ -> Nothing

lexExp :: Input -> (Tokens, Input)
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

recurseLex :: Token -> Input -> (Tokens, Input)
recurseLex token string = (token : tokens, string')
  where
    (tokens, string') = lexExp string

constantMatch :: Input -> Maybe (Constant, Input)
constantMatch string = case reads string :: [(Int, String)] of
  [(int, string')] -> Just (Z int, string')
  _ -> case readFloat string :: [(Rational, String)] of
    [(rational, string')] -> Just (toQ rational, string')
    _ -> Nothing

operatorMatch :: Input -> Maybe (Operator, Input)
operatorMatch string = foldl match empty symbolTable
  where
    match matches (token, operator)
      = matches <|> (,) token <$> patternMatch string operator

patternMatch :: String -> String -> Maybe String
patternMatch (char : string) (char' : pattern)
  | char == char' = patternMatch string pattern
patternMatch string [] = Just string
patternMatch _ _ = Nothing