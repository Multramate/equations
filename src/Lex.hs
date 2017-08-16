module Lex where

import Control.Applicative ((<|>), empty)
import Data.Char (isAlpha, isSpace)
import Data.Maybe (listToMaybe)
import Data.Tuple.Extra (first)
import Numeric (readFloat)

import Equation

--------------------------------------------------------------------------------

-- Lexes an input string into a tuple of token lists
lexEqn :: Input -> Either String (Tokens, Tokens)
lexEqn input = case lexExp input of
  Right (toks @ (_ : _), '=' : input') -> case lexExp input' of
    Right (toks' @ (_ : _), []) -> return (toks, toks')
    Left err -> Left err
    err -> Left "Lexical error: error in RHS"
  Left err -> Left err
  err -> Left "Lexical error: error in LHS"

-- Lexes an input string with an initial minus sign into a token list
lexExp :: Input -> Either String (Tokens, Input)
lexExp input = case lexExp' input of
  Right (Opr Sub : toks, input') -> return (toks', input')
    where
      toks' = Num (Z (-1)) : Opr Jux : toks
  exp -> exp

-- Lexes an input string with an equals sign into a token list
lexExp' :: Input -> Either String (Tokens, Input)
lexExp' input' @ (char : input)
  | char == '=' = return ([], input')
  | char == ',' = recurseLex Sep input
  | char == '(' = recurseLex Opn input
  | char == ')' = recurseLex Cls input
  | isSpace char = lexExp' input
  | otherwise = getMatch >>= uncurry recurseLex
  where
    recurseLex = (. lexExp') . fmap . first . rewriteToks
    getMatch = case foldr ((<|>) . ($ input')) empty matchList of
      Just match -> return match
      _ -> Left $ "Lexical error: unknown character " ++ [char]
    matchList = [matchFun, matchOpr, matchNum, matchChr]
lexExp' _ = return ([], [])

--------------------------------------------------------------------------------

-- Rewrites a token list for certain juxtaposed tokens
rewriteToks :: Token -> Tokens -> Tokens
rewriteToks tok (Opr Sub : toks)
  | any ($ tok) isNeg = tok : Num (Z (-1)) : Opr Jux : toks
  where
    isNeg = [isOperator, (== Sep), (== Opn)]
rewriteToks tok toks @ (tok' : _)
  | any ($ tok) isLeftTerm && any ($ tok') isRightTerm = tok : Opr Mul : toks
  where
    isLeftTerm = [isNumeric, isCharacter, (== Cls)]
    isRightTerm = [isNumeric, isCharacter, isFunction, (== Opn)]
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
  | isAlpha char = return (Chr char, input)
matchChr _ = Nothing

-- Matches an input string with possible operators
matchOpr :: Input -> Maybe (Token, Input)
matchOpr = fmap (first Opr) . flip matchSymbol operatorTable

-- Matches an input string with possible functions
matchFun :: Input -> Maybe (Token, Input)
matchFun input = case fmap (first Fun) (matchSymbol input functionTable) of
  Just (tok, input' @ ('(' : _)) -> return (tok, input')
  Just (tok, input') -> return (tok, '(' : ')' : input')
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
matchPattern input [] = return input
matchPattern _ _ = Nothing