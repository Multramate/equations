module Lexer where

import Control.Applicative ((<|>), empty)
import Data.Char (isAlpha, isSpace)
import Data.Maybe (fromJust, listToMaybe)
import Data.Tuple.Extra (first)
import Numeric (readFloat)

import Equation
import Rational

--------------------------------------------------------------------------------

lexEqn :: Input -> Maybe Equation
lexEqn input = case lexExp input of
  (tokens @ (_ : _), '=' : input') -> case lexExp input' of
    (tokens' @ (_ : _), []) -> Just (Eqn tokens tokens')
    _ -> Nothing
  _ -> Nothing

lexExp :: Input -> (Tokens, Input)
lexExp input = case lexExp' input of
  (Opr Sub : tokens, input') -> (Con (Z (-1)) : Opr Jux : tokens, input')
  (tokens, input') -> (tokens, input')

lexExp' :: Input -> (Tokens, Input)
lexExp' input' @ (char : input)
  | char == '=' = ([], input')
  | char == ',' = recurseLex Sep input
  | char == '(' = recurseLex Opn input
  | char == ')' = recurseLex Cls input
  | isSpace char = lexExp' input
  | otherwise = uncurry recurseLex (getMatch matchList)
  where
    recurseLex = (. lexExp') . first . tokenRewrite
    getMatch = fromJust . foldr ((<|>) . ($ input')) empty
    matchList = [constantMatch, functionMatch, operatorMatch, parameterMatch]
lexExp' _ = ([], [])

--------------------------------------------------------------------------------

tokenRewrite :: Token -> Tokens -> Tokens
tokenRewrite token tokens' @ (Opr Sub : tokens)
  | any ($ token) isNeg = tokens''
  where
    isNeg = [isOperator, (== Sep), (== Opn)]
    tokens'' = token : Con (Z (-1)) : Opr Jux : tokens
tokenRewrite token tokens @ (token' : _)
  | any ($ token) isJuxLeft && any ($ token') isJuxRight = tokens'
  where
    isJuxLeft = [isConstant, isParameter, isVariable, (== Cls)]
    isJuxRight = [isConstant, isParameter, isVariable, isFunction, (== Opn)]
    tokens' = token : Opr Jux : tokens
tokenRewrite token tokens = token : tokens

constantMatch :: Input -> Maybe (Token, Input)
constantMatch input = listToMaybe zList <|> listToMaybe qList
  where
    zList = map (first (Con . Z)) (reads input :: [(Int, String)])
    qList = map (first (Con . toQ)) (readFloat input :: [(Rational, String)])

parameterMatch :: Input -> Maybe (Token, Input)
parameterMatch (char : input)
  | isAlpha char = Just (Par char, input)
parameterMatch (_ : input) = Just (Not, input)
parameterMatch _ = Nothing

operatorMatch :: Input -> Maybe (Token, Input)
operatorMatch = fmap (first Opr) . flip symbolMatch operatorTable

functionMatch :: Input -> Maybe (Token, Input)
functionMatch input = case fmap (first Fun) (symbolMatch input functionTable) of
  Just (token, input' @ ('(' : _)) -> Just (token, input')
  Just (token, input') -> Just (token, '(' : ')' : input')
  _ -> Nothing

--------------------------------------------------------------------------------

isConstant :: Token -> Bool
isConstant (Con _) = True
isConstant _ = False

isParameter :: Token -> Bool
isParameter (Par _) = True
isParameter _ = False

isVariable :: Token -> Bool
isVariable (Var _) = True
isVariable _ = False

isOperator :: Token -> Bool
isOperator (Opr _) = True
isOperator _ = False

isFunction :: Token -> Bool
isFunction (Fun _) = True
isFunction _ = False

symbolMatch :: Input -> [(a, (Symbol, b))] -> Maybe (a, Input)
symbolMatch input = foldl match empty
  where
    match matches (token, (symbol, _))
      = matches <|> (,) token <$> patternMatch input symbol

patternMatch :: String -> String -> Maybe String
patternMatch (char : input) (char' : pattern)
  | char == char' = patternMatch input pattern
patternMatch input [] = Just input
patternMatch _ _ = Nothing