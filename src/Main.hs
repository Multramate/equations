module Main where

import Data.Char (isSpace)
import Data.List (intersperse)

import Equation
import Lexer
import Parser
import Show
import Solver

--------------------------------------------------------------------------------

-- The main function
main :: IO ()
main = do
  putStrLn $ "\n*Enter equation to solve:"
  putStr $ "\t> "
  input <- getLine
  putStrLn $ "\n*Enter variables (default is x):"
  putStr $ "\t> "
  vars <- fmap (filter (not . isSpace)) getLine
  putStrLn $ "\n*Lexing and parsing input with environment ..."
  let env = if null vars then "x" else vars
  case analyse env input of
    Just eqn @ (Eqn lhs rhs) -> do
      putStrLn $ "\n*... equation analysis success:"
      putStrLn $ "\tEquation is " ++ showEqn eqn
      putStrLn $ "\tVariables are " ++ intersperse ' ' env
      putStrLn $ "\n*Proceed to solving ..."
      putStrLn $ "\n*... solution success:"
      putStrLn $ "\t" ++ showEqn (Eqn lhs rhs)
    _ -> putStrLn $ "\n*... equation analysis failure: lexical and parse error"

-- Analyses an input
analyse :: Environment -> Input -> Maybe Equation
analyse env input = parseEqn env =<< lexEqn input