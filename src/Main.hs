module Main where

import Data.Char (isSpace)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)

import Equation
import Lexer
import Parser
import Show
import Solver

--------------------------------------------------------------------------------

-- The main function
main :: IO ()
main = do
  putStr " * Enter equation to solve:\n * > "
  input <- getLine
  putStr " * Enter variables (default is x):\n * > "
  vars <- fmap (filter (not . isSpace)) getLine
  let env = if null vars then "x" else vars
  putStr $ " * Variable(s): " ++ intersperse ' ' env ++ ".\n * Solving ...\n * "
  putStrLn $ case analyse env input of
    Just eqn -> case solve eqn of
      Just eqn' -> '>' : ' ' : showEqn eqn' ++ "\n * ... done."
      _ -> ".. failure: solve error"
    _ -> "... failure: analyse error"

-- Analyses an equation
analyse :: Environment -> Input -> Maybe Equation
analyse env input = parseEqn env =<< lexEqn input