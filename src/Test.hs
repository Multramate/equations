module Test where

import Test.HUnit

import Equation
import Lexer
import Parser

--------------------------------------------------------------------------------

testInput :: Input
testInput = "f(x) / log3(4*5) = (ax^2 - (b*x + c1)) / (6.7 ^ -8.9)"

testEqn :: Equation
testEqn = Eqn
  [ Var 'f', Opn, Var 'x', Cls, Opr Div, Opr Log
  , Con (Z 3), Opn, Con (Z 4), Opr Mul, Con (Z 5), Cls ]
  [ Opn, Var 'a', Var 'x', Opr Exp, Con (Z 2), Opr Sub, Opn
  , Var 'b', Opr Mul, Var 'x', Opr Add, Var 'c', Con (Z 1), Cls, Cls
  , Opr Div, Opn, Con (Q 67 10), Opr Exp, Opr Sub, Con (Q 89 10), Cls ]

testInput' :: Input
testInput' = "f / log3(4*5) ^ (a^2 - (b*x + c)) / 6.7 ^ (0-8.9)"

testRPN :: Queue
testRPN =
  [ Var 'f', Con (Z 3), Con (Z 4), Con (Z 5)
  , Opr Mul, Opr Log, Var 'a', Con (Z 2), Opr Exp, Var 'b'
  , Var 'x', Opr Mul, Var 'c', Opr Add, Opr Sub, Opr Exp, Opr Div
  , Con (Q 67 10), Con (Z 0), Con (Q 89 10), Opr Sub, Opr Exp, Opr Div ]

--------------------------------------------------------------------------------

lexExp' :: Input -> Tokens
lexExp' = fst . lexExp

convertRPN' :: Input -> Maybe Queue
convertRPN' = convertRPN . lexExp'

--------------------------------------------------------------------------------

tests :: Test
tests = TestList
  [lexerTests, parserTests]

--------------------------------------------------------------------------------

lexerTests :: Test
lexerTests = TestList
  [ lexEqn " " ~?= Nothing
  , lexEqn "=" ~?= Nothing
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
  , lexEqn "x = y" ~?= Just (Eqn [Var 'x'] [Var 'y'])
  , lexExp' "1" ~?= [Con (Z 1)]
  , lexExp' "1.2" ~?= [Con (Q 6 5)]
  , lexExp' "x" ~?= [Var 'x']
  , lexExp' "xy" ~?= [Var 'x', Var 'y']
  , lexExp' "x y" ~?= [Var 'x', Var 'y']
  , lexExp' "+" ~?= [Opr Add]
  , lexExp' "1.2*x" ~?= [Con (Q 6 5), Opr Mul, Var 'x']
  , lexExp' "log" ~?= [Opr Log]
  , lexExp' "1.2logx" ~?= [Con (Q 6 5), Opr Log, Var 'x']
  , lexExp' "(log)" ~?= [Opn, Opr Log, Cls]
  , lexExp' "log^log" ~?= [Opr Log, Opr Exp, Opr Log]
  , lexExp' "?" ~?= [Not]
  , constantMatch "" ~?= Nothing
  , constantMatch " " ~?= Nothing
  , constantMatch "x" ~?= Nothing
  , constantMatch "x1" ~?= Nothing
  , constantMatch "1" ~?= Just (Z 1, "")
  , constantMatch "1.2" ~?= Just (Q 6 5, "")
  , constantMatch "1+" ~?= Just (Z 1, "+")
  , constantMatch "1.2+" ~?= Just (Q 6 5, "+")
  , constantMatch "1.2.3" ~?= Just (Q 6 5, ".3")
  , operatorMatch "" ~?= Nothing
  , operatorMatch " " ~?= Nothing
  , operatorMatch "x" ~?= Nothing
  , operatorMatch "x+" ~?= Nothing
  , operatorMatch "+x" ~?= Just (Add, "x")
  , operatorMatch "logx" ~?= Just (Log, "x")
  , patternMatch "" "" ~?= Just ""
  , patternMatch "" "x" ~?= Nothing
  , patternMatch "x" "" ~?= Just "x"
  , patternMatch "x" "x" ~?= Just ""
  , patternMatch "x" "xy" ~?= Nothing
  , patternMatch "xy" "x" ~?= Just "y"
  , patternMatch "xy" "xy" ~?= Just ""
  , lexEqn testInput ~?= Just testEqn
  ]

--------------------------------------------------------------------------------

parserTests :: Test
parserTests = TestList
  [ convertRPN' "(" ~?= Nothing
  , convertRPN' ")" ~?= Nothing
  , convertRPN' ")(" ~?= Nothing
  , convertRPN' "" ~?= Just []
  , convertRPN' "()" ~?= Just []
  , convertRPN' "(())" ~?= Just []
  , convertRPN' "()()" ~?= Just []
  , convertRPN' "x+y" ~?= Just [Var 'x', Var 'y', Opr Add]
  , convertRPN' "xy+" ~?= Just [Var 'x', Var 'y', Opr Add]
  , convertRPN' "+xy" ~?= Just [Var 'x', Var 'y', Opr Add]
  , convertRPN' "x+y+z" ~?= Just [Var 'x', Var 'y', Opr Add, Var 'z', Opr Add]
  , convertRPN' "(x+y)+z" ~?= Just [Var 'x', Var 'y', Opr Add, Var 'z', Opr Add]
  , convertRPN' "x+(y+z)" ~?= Just [Var 'x', Var 'y', Var 'z', Opr Add, Opr Add]
  , convertRPN' "x+y*z" ~?= Just [Var 'x', Var 'y', Var 'z', Opr Mul, Opr Add]
  , convertRPN' "(x+y)*z" ~?= Just [Var 'x', Var 'y', Opr Add, Var 'z', Opr Mul]
  , convertRPN' "x+(y*z)" ~?= Just [Var 'x', Var 'y', Var 'z', Opr Mul, Opr Add]
  , convertRPN' "x*y+z" ~?= Just [Var 'x', Var 'y', Opr Mul, Var 'z', Opr Add]
  , convertRPN' "(x*y)+z" ~?= Just [Var 'x', Var 'y', Opr Mul, Var 'z', Opr Add]
  , convertRPN' "x*(y+z)" ~?= Just [Var 'x', Var 'y', Var 'z', Opr Add, Opr Mul]
  , convertRPN' "x^y" ~?= Just [Var 'x', Var 'y', Opr Exp]
  , convertRPN' "^xy" ~?= Just [Var 'x', Var 'y', Opr Exp]
  , convertRPN' "xy^" ~?= Just [Var 'x', Var 'y', Opr Exp]
  , convertRPN' "x^y^z" ~?= Just [Var 'x', Var 'y', Var 'z', Opr Exp, Opr Exp]
  , convertRPN' "(x^y)^z" ~?= Just [Var 'x', Var 'y', Opr Exp, Var 'z', Opr Exp]
  , convertRPN' "x^(y^z)" ~?= Just [Var 'x', Var 'y', Var 'z', Opr Exp, Opr Exp]
  , convertRPN' "x+y^z" ~?= Just [Var 'x', Var 'y', Var 'z', Opr Exp, Opr Add]
  , convertRPN' "x+(y^z)" ~?= Just [Var 'x', Var 'y', Var 'z', Opr Exp, Opr Add]
  , convertRPN' "(x+y)^z" ~?= Just [Var 'x', Var 'y', Opr Add, Var 'z', Opr Exp]
  , convertRPN' "logxy" ~?= Just [Var 'x', Var 'y', Opr Log]
  , convertRPN' "xlogy" ~?= Just [Var 'x', Var 'y', Opr Log]
  , convertRPN' "xylog" ~?= Just [Var 'x', Var 'y', Opr Log]
  , convertRPN' "logxyz" ~?= Just [Var 'x', Var 'y', Var 'z', Opr Log]
  , convertRPN' "xlogyz" ~?= Just [Var 'x', Var 'y', Var 'z', Opr Log]
  , convertRPN' "xylogz" ~?= Just [Var 'x', Var 'y', Var 'z', Opr Log]
  , convertRPN' "xyzlog" ~?= Just [Var 'x', Var 'y', Var 'z', Opr Log]
  , convertRPN' testInput' ~?= Just testRPN
  ]