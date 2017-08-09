module Show where

import Data.List (intercalate)

import Equation

--------------------------------------------------------------------------------

-- Shows an equation
showEqn :: Equation -> String
showEqn (Eqn exp exp') = showExp exp ++ "=" ++ showExp exp'

-- Shows a token
showTok :: Token -> String
showTok (Num num) = showNum num
showTok (Chr chr) = [chr]
showTok (Opr opr) = showOpr opr
showTok (Fun fun) = showFun fun
showTok Sep = ","
showTok Opn = "("
showTok Cls = ")"

-- Shows an expression
showExp :: Expression -> String
showExp (Val val) = showNum val
showExp (Con con) = [con]
showExp (Var var) = [var]
showExp (Bin opr exp exp')
  = "(" ++ showExp exp ++ showOpr opr ++ showExp exp' ++ ")"  
showExp (App fun exps)
  = showFun fun ++ "(" ++ intercalate "," (map showExp exps) ++ ")"

-- Shows a numeric
showNum :: Numeric -> String
showNum (Z z) = show z
showNum (Q n d) = "(" ++ show n ++ "/" ++ show d ++ ")"
showNum (R r) = show r

-- Shows an operator
showOpr :: Operator -> String
showOpr = getOperatorSymbol

-- Shows a Function
showFun :: Function -> String
showFun = getFunctionSymbol