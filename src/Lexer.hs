module Lexer where

import Control.Applicative ((<|>), empty)
import Data.Char (isAlpha, isSpace)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Tuple.Extra (first)
import Numeric (readFloat)

import Equation
import Rational

--------------------------------------------------------------------------------

lexEqn :: Input -> Maybe (Tokens, Tokens)
lexEqn input = case lexExp input of
  Just (tokens @ (_ : _), '=' : input') -> case lexExp input' of
    Just (tokens' @ (_ : _), []) -> Just (tokens, tokens')
    _ -> Nothing
  _ -> Nothing

lexExp :: Input -> Maybe (Tokens, Input)
lexExp input = case lexExp' input of
  Just (Opr Sub : tokens, input') -> Just (tokens', input')
    where
      tokens' = Num (Z (-1)) : Opr Jux : tokens
  exp -> exp

lexExp' :: Input -> Maybe (Tokens, Input)
lexExp' input' @ (char : input)
  | char == '=' = Just ([], input')
  | char == ',' = recurseLex Sep input
  | char == '(' = recurseLex Opn input
  | char == ')' = recurseLex Cls input
  | isSpace char = lexExp' input
  | otherwise = uncurry recurseLex =<< getMatch matchList
  where
    recurseLex = (. lexExp') . fmap . first . tokenRewrite
    getMatch = foldr ((<|>) . ($ input')) empty
    matchList = [functionMatch, operatorMatch, numericMatch, characterMatch]
lexExp' _ = Just ([], [])

--------------------------------------------------------------------------------

tokenRewrite :: Token -> Tokens -> Tokens
tokenRewrite token tokens' @ (Opr Sub : tokens)
  | any ($ token) isNeg = tokens''
  where
    isNeg = [isOperator, (== Sep), (== Opn)]
    tokens'' = token : Num (Z (-1)) : Opr Jux : tokens
tokenRewrite token tokens @ (token' : _)
  | any ($ token) isJuxLeft && any ($ token') isJuxRight = tokens'
  where
    isJuxLeft = [isNumeric, isCharacter, (== Cls)]
    isJuxRight = [isNumeric, isCharacter, isFunction, (== Opn)]
    tokens' = token : Opr Jux : tokens
tokenRewrite token tokens = token : tokens

numericMatch :: Input -> Maybe (Token, Input)
numericMatch input = listToMaybe zList <|> listToMaybe qList
  where
    zList = map (first (Num . Z)) (reads input :: [(Int, String)])
    qList = map (first (Num . toQ)) (readFloat input :: [(Rational, String)])

characterMatch :: Input -> Maybe (Token, Input)
characterMatch (char : input)
  | isAlpha char = Just (Chr char, input)
characterMatch (_ : input) = Nothing
characterMatch _ = Nothing

operatorMatch :: Input -> Maybe (Token, Input)
operatorMatch = fmap (first Opr) . flip symbolMatch operatorTable

functionMatch :: Input -> Maybe (Token, Input)
functionMatch input = case fmap (first Fun) (symbolMatch input functionTable) of
  Just (token, input' @ ('(' : _)) -> Just (token, input')
  Just (token, input') -> Just (token, '(' : ')' : input')
  _ -> Nothing

--------------------------------------------------------------------------------

isNumeric :: Token -> Bool
isNumeric (Num _) = True
isNumeric _ = False

isCharacter :: Token -> Bool
isCharacter (Chr _) = True
isCharacter _ = False

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