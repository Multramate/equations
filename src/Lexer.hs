module Lexer where

import Control.Applicative ((<|>), empty)
import Data.Char (isAlpha, isSpace)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Tuple.Extra (first)
import Numeric (readFloat)

import Equation

--------------------------------------------------------------------------------

-- Lexes an input string into a tuple of token lists
lexEqn :: Input -> Maybe (Tokens, Tokens)
lexEqn input = case lexExp input of
  Just (toks @ (_ : _), '=' : input') -> case lexExp input' of
    Just (toks' @ (_ : _), []) -> Just (toks, toks')
    _ -> Nothing
  _ -> Nothing

-- Lexes an input string with an initial minus sign into a token list
lexExp :: Input -> Maybe (Tokens, Input)
lexExp input = case lexExp' input of
  Just (Opr Sub : toks, input') -> Just (Num (Z (-1)) : Opr Jux : toks, input')
  exp -> exp

-- Lexes an input string with an equals sign into a token list
lexExp' :: Input -> Maybe (Tokens, Input)
lexExp' input' @ (char : input)
  | char == '=' = Just ([], input')
  | char == ',' = recurseLex Sep input
  | char == '(' = recurseLex Opn input
  | char == ')' = recurseLex Cls input
  | isSpace char = lexExp' input
  | otherwise = uncurry recurseLex =<< getMatch matchList
  where
    recurseLex = (. lexExp') . fmap . first . rewriteToks
    getMatch = foldr ((<|>) . ($ input')) empty
    matchList = [matchFun, matchOpr, matchNum, matchChr]
lexExp' _ = Just ([], [])

--------------------------------------------------------------------------------

-- Rewrites a tokens list for certain juxtaposed tokens
rewriteToks :: Token -> Tokens -> Tokens
rewriteToks tok toks' @ (Opr Sub : toks)
  | any ($ tok) isNeg = tok : Num (Z (-1)) : Opr Jux : toks
  where
    isNeg = [isOperator, (== Sep), (== Opn)]
rewriteToks tok toks @ (tok' : _)
  | any ($ tok) isJuxLeft && any ($ tok') isJuxRight = tok : Opr Jux : toks
  where
    isJuxLeft = [isNumeric, isCharacter, (== Cls)]
    isJuxRight = [isNumeric, isCharacter, isFunction, (== Opn)]
rewriteToks tok toks = tok : toks

-- Matches an input string with possible numerics
matchNum :: Input -> Maybe (Token, Input)
matchNum input = listToMaybe zList <|> listToMaybe qList
  where
    zList = map (first (Num . Z)) (reads input :: [(Int, String)])
    qList = map (first (Num . toQ)) (readFloat input :: [(Rational, String)])

-- Matches an input string with possible characters
matchChr :: Input -> Maybe (Token, Input)
matchChr (char : input)
  | isAlpha char = Just (Chr char, input)
matchChr _ = Nothing

-- Matches an input string with possible operators
matchOpr :: Input -> Maybe (Token, Input)
matchOpr = fmap (first Opr) . flip matchSymbol operatorTable

-- Matches an input string with possible functions
matchFun :: Input -> Maybe (Token, Input)
matchFun input = case fmap (first Fun) (matchSymbol input functionTable) of
  Just (tok, input' @ ('(' : _)) -> Just (tok, input')
  Just (tok, input') -> Just (tok, '(' : ')' : input')
  _ -> Nothing

--------------------------------------------------------------------------------

-- Checks if a token is a numeric
isNumeric :: Token -> Bool
isNumeric (Num _) = True
isNumeric _ = False

-- Checks if a token is a character
isCharacter :: Token -> Bool
isCharacter (Chr _) = True
isCharacter _ = False

-- Checks if a token is an operator
isOperator :: Token -> Bool
isOperator (Opr _) = True
isOperator _ = False

-- Checks if a token is a function
isFunction :: Token -> Bool
isFunction (Fun _) = True
isFunction _ = False

-- Matches an input string with possible symbols
matchSymbol :: Input -> [(a, (Symbol, b))] -> Maybe (a, Input)
matchSymbol input = foldl match empty
  where
    match matches (tok, (symbol, _))
      = matches <|> (,) tok <$> matchPattern input symbol

-- Matches a string with a possible pattern
matchPattern :: String -> String -> Maybe String
matchPattern (char : input) (char' : pattern)
  | char == char' = matchPattern input pattern
matchPattern input [] = Just input
matchPattern _ _ = Nothing