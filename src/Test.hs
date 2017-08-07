module Test where

import Test.HUnit ((~?=), Test (TestList), runTestTT)

import Equation
import Lexer
import Parser

--------------------------------------------------------------------------------

lexExp'' :: Input -> Tokens
lexExp'' = fst . lexExp

convertRPN' :: Input -> Maybe Queue
convertRPN' = convertRPN . lexExp''

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
  , lexEqn "x=y" ~?= Just (Eqn [Par 'x'] [Par 'y'])
  , lexEqn " x = y " ~?= Just (Eqn [Par 'x'] [Par 'y'])
  , lexEqn "-x=-y" ~?= Just (Eqn [Con (Z (-1)), Opr Jux, Par 'x'] [Con (Z (-1)), Opr Jux, Par 'y'])
  , lexEqn " - x = - y " ~?= Just (Eqn [Con (Z (-1)), Opr Jux, Par 'x'] [Con (Z (-1)), Opr Jux, Par 'y'])
  , lexExp'' "1" ~?= [Con (Z 1)]
  , lexExp'' "1.2" ~?= [Con (Q 6 5)]
  , lexExp'' "1 2" ~?= [Con (Z 1), Opr Jux, Con (Z 2)]
  , lexExp'' "1.2 3.4" ~?= [Con (Q 6 5), Opr Jux, Con (Q 17 5)]
  , lexExp'' "x" ~?= [Par 'x']
  , lexExp'' "xy" ~?= [Par 'x', Opr Jux, Par 'y']
  , lexExp'' "x y" ~?= [Par 'x', Opr Jux, Par 'y']
  , lexExp'' "1.2x" ~?= [Con (Q 6 5), Opr Jux, Par 'x']
  , lexExp'' "x1.2" ~?= [Par 'x', Opr Jux, Con (Q 6 5)]
  , lexExp'' "+*^" ~?= [Opr Add, Opr Mul, Opr Exp]
  , lexExp'' "log" ~?= [Fun Log, Opn, Cls]
  , lexExp'' "1.2log" ~?= [Con (Q 6 5), Opr Jux, Fun Log, Opn, Cls]
  , lexExp'' "log1.2" ~?= [Fun Log, Opn, Cls, Opr Jux, Con (Q 6 5)]
  , lexExp'' "xlog" ~?= [Par 'x', Opr Jux, Fun Log, Opn, Cls]
  , lexExp'' "logx" ~?= [Fun Log, Opn, Cls, Opr Jux, Par 'x']
  , lexExp'' "(,)?" ~?= [Opn, Sep, Cls, Not]
  , lexExp'' "1.2(" ~?= [Con (Q 6 5), Opr Jux, Opn]
  , lexExp'' "x(" ~?= [Par 'x', Opr Jux, Opn]
  , lexExp'' ")(" ~?= [Cls, Opr Jux, Opn]
  , lexExp'' "-2.3" ~?= [Con (Z (-1)), Opr Jux, Con (Q 23 10)]
  , lexExp'' "1-2.3" ~?= [Con (Z 1), Opr Sub, Con (Q 23 10)]
  , lexExp'' "x-2.3" ~?= [Par 'x', Opr Sub, Con (Q 23 10)]
  , lexExp'' "+-2.3" ~?= [Opr Add, Con (Z (-1)), Opr Jux, Con (Q 23 10)]
  , lexExp'' "log-2.3" ~?= [Fun Log, Opn, Cls, Opr Sub, Con (Q 23 10)]
  , lexExp'' ",-2.3" ~?= [Sep, Con (Z (-1)), Opr Jux, Con (Q 23 10)]
  , lexExp'' "(-2.3" ~?= [Opn, Con (Z (-1)), Opr Jux, Con (Q 23 10)]
  , lexExp'' ")-2.3" ~?= [Cls, Opr Sub, Con (Q 23 10)]
  , lexExp'' "?-2.3" ~?= [Not, Opr Sub, Con (Q 23 10)]
  , constantMatch "" ~?= Nothing
  , constantMatch " " ~?= Nothing
  , constantMatch "x" ~?= Nothing
  , constantMatch "x1" ~?= Nothing
  , constantMatch "1" ~?= Just (Con (Z 1), "")
  , constantMatch "1.2" ~?= Just (Con (Q 6 5), "")
  , constantMatch "1+" ~?= Just (Con (Z 1), "+")
  , constantMatch "1.2+" ~?= Just (Con (Q 6 5), "+")
  , constantMatch "1.2.3" ~?= Just (Con (Q 6 5), ".3")
  , constantMatch " 1 .2" ~?= Just (Con (Z 1), " .2")
  , parameterMatch "" ~?= Nothing
  , parameterMatch " " ~?= Just (Not, "")
  , parameterMatch " x" ~?= Just (Not, "x")
  , parameterMatch "1" ~?= Just (Not, "")
  , parameterMatch "1x" ~?= Just (Not, "x")
  , parameterMatch "x" ~?= Just (Par 'x', "")
  , parameterMatch "x1" ~?= Just (Par 'x', "1")
  , operatorMatch "" ~?= Nothing
  , operatorMatch " " ~?= Nothing
  , operatorMatch "x" ~?= Nothing
  , operatorMatch "x+" ~?= Nothing
  , operatorMatch " +" ~?= Nothing
  , operatorMatch "+x" ~?= Just (Opr Add, "x")
  , operatorMatch "++" ~?= Just (Opr Add, "+")
  , functionMatch "" ~?= Nothing
  , functionMatch " " ~?= Nothing
  , functionMatch "x" ~?= Nothing
  , functionMatch "xlog" ~?= Nothing
  , functionMatch " l o g " ~?= Nothing
  , functionMatch " log log " ~?= Nothing
  , functionMatch "logx" ~?= Just (Fun Log, "()x")
  , functionMatch "log(x)" ~?= Just (Fun Log, "(x)")
  , functionMatch "loglog" ~?= Just (Fun Log, "()log")
  , patternMatch "" "" ~?= Just ""
  , patternMatch "" "x" ~?= Nothing
  , patternMatch "x" "" ~?= Just "x"
  , patternMatch "x" "x" ~?= Just ""
  , patternMatch "x" "xy" ~?= Nothing
  , patternMatch "xy" "x" ~?= Just "y"
  , patternMatch "xy" "xy" ~?= Just ""
  ]

--------------------------------------------------------------------------------

parserTests :: Test
parserTests = TestList
  [ convertRPN' "(" ~?= Nothing
  , convertRPN' ")" ~?= Nothing
  , convertRPN' ")(" ~?= Nothing
  , convertRPN' "," ~?= Nothing
  , convertRPN' "(," ~?= Nothing
  , convertRPN' ",)" ~?= Nothing
  , convertRPN' "" ~?= Just []
  , convertRPN' "(())" ~?= Just []
  , convertRPN' "()()" ~?= Just [Opr Jux]
  , convertRPN' "(,)" ~?= Just []
  , convertRPN' "x+y" ~?= Just [Par 'x', Par 'y', Opr Add]
  , convertRPN' "xy+" ~?= Just [Par 'x', Par 'y', Opr Jux, Opr Add]
  , convertRPN' "+xy" ~?= Just [Par 'x', Par 'y', Opr Jux, Opr Add]
  , convertRPN' "x+y+z" ~?= Just [Par 'x', Par 'y', Opr Add, Par 'z', Opr Add]
  , convertRPN' "(x+y)+z" ~?= Just [Par 'x', Par 'y', Opr Add, Par 'z', Opr Add]
  , convertRPN' "x+(y+z)" ~?= Just [Par 'x', Par 'y', Par 'z', Opr Add, Opr Add]
  , convertRPN' "x+y*z" ~?= Just [Par 'x', Par 'y', Par 'z', Opr Mul, Opr Add]
  , convertRPN' "(x+y)*z" ~?= Just [Par 'x', Par 'y', Opr Add, Par 'z', Opr Mul]
  , convertRPN' "x+(y*z)" ~?= Just [Par 'x', Par 'y', Par 'z', Opr Mul, Opr Add]
  , convertRPN' "x*y+z" ~?= Just [Par 'x', Par 'y', Opr Mul, Par 'z', Opr Add]
  , convertRPN' "(x*y)+z" ~?= Just [Par 'x', Par 'y', Opr Mul, Par 'z', Opr Add]
  , convertRPN' "x*(y+z)" ~?= Just [Par 'x', Par 'y', Par 'z', Opr Add, Opr Mul]
  , convertRPN' "x^y" ~?= Just [Par 'x', Par 'y', Opr Exp]
  , convertRPN' "^xy" ~?= Just [Par 'x', Par 'y', Opr Jux, Opr Exp]
  , convertRPN' "xy^" ~?= Just [Par 'x', Par 'y', Opr Jux, Opr Exp]
  , convertRPN' "x^y^z" ~?= Just [Par 'x', Par 'y', Par 'z', Opr Exp, Opr Exp]
  , convertRPN' "(x^y)^z" ~?= Just [Par 'x', Par 'y', Opr Exp, Par 'z', Opr Exp]
  , convertRPN' "x^(y^z)" ~?= Just [Par 'x', Par 'y', Par 'z', Opr Exp, Opr Exp]
  , convertRPN' "x*y^z" ~?= Just [Par 'x', Par 'y', Par 'z', Opr Exp, Opr Mul]
  , convertRPN' "x*(y^z)" ~?= Just [Par 'x', Par 'y', Par 'z', Opr Exp, Opr Mul]
  , convertRPN' "(x*y)^z" ~?= Just [Par 'x', Par 'y', Opr Mul, Par 'z', Opr Exp]
  , convertRPN' "log" ~?= Just [Fun Log]
  , convertRPN' "logx" ~?= Just [Fun Log, Par 'x', Opr Jux]
  , convertRPN' "logxy" ~?= Just [Fun Log, Par 'x', Opr Jux, Par 'y', Opr Jux]
  , convertRPN' "xlogy" ~?= Just [Par 'x', Fun Log, Opr Jux, Par 'y', Opr Jux]
  , convertRPN' "xylog" ~?= Just [Par 'x', Par 'y', Opr Jux, Fun Log, Opr Jux]
  , convertRPN' "log(x)" ~?= Just [Par 'x', Fun Log]
  , convertRPN' "log(x,y)" ~?= Just [Par 'x', Par 'y', Fun Log]
  , convertRPN' "x+logy" ~?= Just [Par 'x', Fun Log, Par 'y', Opr Jux, Opr Add]
  , convertRPN' "(x+log)y" ~?= Just [Par 'x', Fun Log, Opr Add, Par 'y', Opr Jux]
  , convertRPN' "x+log(y)" ~?= Just [Par 'x', Par 'y', Fun Log, Opr Add]
  , convertRPN' "logx+y" ~?= Just [Fun Log, Par 'x', Opr Jux, Par 'y', Opr Add]
  , convertRPN' "log(x+y)" ~?= Just [Par 'x', Par 'y', Opr Add, Fun Log]
  , convertRPN' "log(x)+y" ~?= Just [Par 'x', Fun Log, Par 'y', Opr Add]
  , convertRPN' "x+logyz" ~?= Just [Par 'x', Fun Log, Par 'y', Opr Jux, Par 'z', Opr Jux, Opr Add]
  , convertRPN' "(x+log)yz" ~?= Just [Par 'x', Fun Log, Opr Add, Par 'y', Opr Jux, Par 'z', Opr Jux]
  , convertRPN' "x+log(y,z)" ~?= Just [Par 'x', Par 'y', Par 'z', Fun Log, Opr Add]
  , convertRPN' "logxy+z" ~?= Just [Fun Log, Par 'x', Opr Jux, Par 'y', Opr Jux, Par 'z', Opr Add]
  , convertRPN' "logx(y+z)" ~?= Just [Fun Log, Par 'x', Opr Jux, Par 'y', Par 'z', Opr Add, Opr Jux]
  , convertRPN' "log(x,y)+z" ~?= Just [Par 'x', Par 'y', Fun Log, Par 'z', Opr Add]
  ]