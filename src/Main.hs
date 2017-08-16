module Main where

import Data.Char (isSpace)
import Data.List (intersperse)

import Equation
import Lex
import Parse
import Show

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
    Right eqn @ (Eqn lhs rhs) -> do
      putStrLn $ "\n*... equation analysis success:"
      putStrLn $ "\tEquation is " ++ showEqn eqn
      putStrLn $ "\tVariables are " ++ intersperse ' ' env
      putStrLn $ "\n*Proceed to solving ..."
      putStrLn $ "\n*... solution success:"
      putStrLn $ "\t" ++ showEqn (Eqn lhs rhs)
    Left err -> putStrLn $ "\n*... equation analysis failure: " ++ err

-- Analyses an input
analyse :: Environment -> Input -> Either String Equation
analyse env input = lexEqn input >>= parseEqn env