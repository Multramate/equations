module Test where

import Test.HUnit ((~?=), Test (TestList), runTestTT)

import Equation
import Lexer
import Main
import Parser
import Show
import Solver

--------------------------------------------------------------------------------

lexExp'' :: Input -> Maybe Tokens
lexExp'' input = return input >>= fmap fst . lexExp

convertRPN' :: Input -> Maybe TokenQueue
convertRPN' input = return input >>= lexExp'' >>= fmap reverse . convertRPN

convertAST' :: Input -> Maybe Expression
convertAST' input = return input >>= lexExp'' >>= convertRPN >>= convertAST []

rewrite' :: Input -> Maybe Expression
rewrite' input = return input >>= convertAST' >>= rewrite

--------------------------------------------------------------------------------

tests :: Test
tests = TestList
  [lexerTests, parserTests, solverTests]

--------------------------------------------------------------------------------

solverTests :: Test
solverTests = TestList
  [ rewrite' "" ~?= Nothing
  , rewrite' "2.3" ~?= Just 2.3
  , rewrite' "-2.3" ~?= Just (Val (Q (-23) 10))
  , rewrite' "x" ~?= Just (Con 'x')
  , rewrite' "-x" ~?= Just (-Con 'x')
  , rewrite' "0+0" ~?= Just 0
  , rewrite' "0+2.3" ~?= Just 2.3
  , rewrite' "2.3+0" ~?= Just 2.3
  , rewrite' "-2.3+2.3" ~?= Just 0
  , rewrite' "2.3+-2.3" ~?= Just 0
  , rewrite' "1.2+3.4" ~?= Just (Val (1.2 + 3.4))
  , rewrite' "0+x" ~?= Just (Con 'x')
  , rewrite' "x+0" ~?= Just (Con 'x')
  , rewrite' "-x+x" ~?= Just 0
  , rewrite' "x+-x" ~?= Just 0
  , rewrite' "x+y" ~?= Just (Bin Add (Con 'x') (Con 'y'))
  , rewrite' "0-0" ~?= Just 0
  , rewrite' "0-2.3" ~?= Just (Val (Q (-23) 10))
  , rewrite' "2.3-0" ~?= Just 2.3
  , rewrite' "2.3-2.3" ~?= Just 0
  , rewrite' "1.2-3.4" ~?= Just (Val (1.2 - 3.4))
  , rewrite' "0-x" ~?= Just (-Con 'x')
  , rewrite' "x-0" ~?= Just (Con 'x')
  , rewrite' "x-x" ~?= Just 0
  , rewrite' "x-y" ~?= Just (Bin Sub (Con 'x') (Con 'y'))
  , rewrite' "0*0" ~?= Just 0
  , rewrite' "0*2.3" ~?= Just 0
  , rewrite' "2.3*0" ~?= Just 0
  , rewrite' "1*2.3" ~?= Just 2.3
  , rewrite' "2.3*1" ~?= Just 2.3
  , rewrite' "-1*2.3" ~?= Just (Val (Q (-23) 10))
  , rewrite' "2.3*-1" ~?= Just (Val (Q (-23) 10))
  , rewrite' "(1/2.3)*2.3" ~?= Just 1
  , rewrite' "2.3*(1/2.3)" ~?= Just 1
  , rewrite' "1.2*3.4" ~?= Just (Val (1.2 * 3.4))
  , rewrite' "0*x" ~?= Just 0
  , rewrite' "x*0" ~?= Just 0
  , rewrite' "1*x" ~?= Just (Con 'x')
  , rewrite' "x*1" ~?= Just (Con 'x')
  , rewrite' "-1*x" ~?= Just (-Con 'x')
  , rewrite' "x*-1" ~?= Just (-Con 'x')
  , rewrite' "(1/x)*x" ~?= Just 1
  , rewrite' "x*(1/x)" ~?= Just 1
  , rewrite' "x*y" ~?= Just (Bin Mul (Con 'x') (Con 'y'))
  , rewrite' "0/0" ~?= Nothing
  , rewrite' "0/2.3" ~?= Just 0
  , rewrite' "2.3/0" ~?= Nothing
  , rewrite' "1/2.3" ~?= Just (Val (Q 10 23))
  , rewrite' "2.3/1" ~?= Just 2.3
  , rewrite' "2.3/2.3" ~?= Just 1
  , rewrite' "-1/2.3" ~?= Just (Val (Q (-10) 23))
  , rewrite' "2.3/-1" ~?= Just (Val (Q (-23) 10))
  , rewrite' "1.2/3.4" ~?= Just (Val (1.2 / 3.4))
  , rewrite' "0/x" ~?= Just 0
  , rewrite' "x/0" ~?= Nothing
  , rewrite' "1/x" ~?= Just (recip (Con 'x'))
  , rewrite' "x/1" ~?= Just (Con 'x')
  , rewrite' "x/x" ~?= Just 1
  , rewrite' "-1/x" ~?= Just (-recip (Con 'x'))
  , rewrite' "x/-1" ~?= Just (-Con 'x')
  , rewrite' "x/y" ~?= Just (Bin Div (Con 'x') (Con 'y'))
  , rewrite' "0^0" ~?= Nothing
  , rewrite' "-2.3^0" ~?= Nothing
  , rewrite' "0^2.3" ~?= Just 0
  , rewrite' "2.3^0" ~?= Just 1
  , rewrite' "1^2.3" ~?= Just 1
  , rewrite' "2.3^1" ~?= Just 2.3
  , rewrite' "2.3^-1" ~?= Just (Val (Q 10 23))
  , rewrite' "-2.3^1" ~?= Nothing
  , rewrite' "-1.2^3.4" ~?= Nothing
  , rewrite' "1.2^3.4" ~?= Just (Val (R (1.2 ** 3.4)))
  , rewrite' "0^x" ~?= Just 0
  , rewrite' "x^0" ~?= Just 1
  , rewrite' "1^x" ~?= Just 1
  , rewrite' "x^1" ~?= Just (Con 'x')
  , rewrite' "x^-1" ~?= Just (recip (Con 'x'))
  , rewrite' "-2.3^x" ~?= Nothing
  , rewrite' "x^x" ~?= Just (Bin Exp (Con 'x') (Con 'x'))
  , rewrite' "0 2.3" ~?= Just 0
  , rewrite' "2.3 0" ~?= Just 0
  , rewrite' "1 2.3" ~?= Just 2.3
  , rewrite' "2.3 1" ~?= Just 2.3
  , rewrite' "-1 2.3" ~?= Just (Val (Q (-23) 10))
  , rewrite' "2.3 (-1)" ~?= Just (Val (Q (-23) 10))
  , rewrite' "(1/2.3) 2.3" ~?= Just 1
  , rewrite' "2.3 (1/2.3)" ~?= Just 1
  , rewrite' "1.2 3.4" ~?= Just (Val (1.2 * 3.4))
  , rewrite' "0x" ~?= Just 0
  , rewrite' "x0" ~?= Just 0
  , rewrite' "1x" ~?= Just (Con 'x')
  , rewrite' "x1" ~?= Just (Con 'x')
  , rewrite' "-1x" ~?= Just (-Con 'x')
  , rewrite' "x(-1)" ~?= Just (-Con 'x')
  , rewrite' "(1/x)x" ~?= Just 1
  , rewrite' "x(1/x)" ~?= Just 1
  , rewrite' "xy" ~?= Just (Bin Jux (Con 'x') (Con 'y'))
  , rewrite' "1+x" ~?= Just (1 + Con 'x')
  , rewrite' "x+1" ~?= Just (Con 'x' + 1)
  , rewrite' "1+(2+3)" ~?= Just 6
  , rewrite' "(1+2)+3" ~?= Just 6
  , rewrite' "(1+2)+(3+4)" ~?= Just 10
  , rewrite' "((1+2)+3)+4" ~?= Just 10
  , rewrite' "(1+2)+x" ~?= Just (3 + Con 'x')
  , rewrite' "x+(1+2)" ~?= Just (Con 'x' + 3)
  , rewrite' "1+(x+2)" ~?= Just (1 + (Con 'x' + 2))
  , rewrite' "(x+1)+2" ~?= Just (Con 'x' + 1 + 2)
  , rewrite' "(1+2)+(x+3)" ~?= Just (3 + (Con 'x' + 3))
  , rewrite' "(x+1)+(2+3)" ~?= Just (Con 'x' + 1 + 5)
  , rewrite' "1+(2+(3+x))" ~?= Just (1 + (2 + (3 + Con 'x')))
  , rewrite' "((x+1)+2)+3" ~?= Just (Con 'x' + 1 + 2 + 3)
  , rewrite' "log(0,0)" ~?= Nothing
  , rewrite' "log(0,4)" ~?= Nothing
  , rewrite' "log(2,0)" ~?= Nothing
  , rewrite' "log(0,x)" ~?= Nothing
  , rewrite' "log(x,0)" ~?= Nothing
  , rewrite' "log(-1,-1)" ~?= Nothing
  , rewrite' "log(0,-1)" ~?= Nothing
  , rewrite' "log(-1,0)" ~?= Nothing
  , rewrite' "log(2,-1)" ~?= Nothing
  , rewrite' "log(-1,4)" ~?= Nothing
  , rewrite' "log(x,-1)" ~?= Nothing
  , rewrite' "log(-1,x)" ~?= Nothing
  , rewrite' "log(1,1)" ~?= Nothing
  , rewrite' "log(0,1)" ~?= Nothing
  , rewrite' "log(1,0)" ~?= Nothing
  , rewrite' "log(-1,1)" ~?= Nothing
  , rewrite' "log(1,-1)" ~?= Nothing
  , rewrite' "log(1,4)" ~?= Nothing
  , rewrite' "log(1,x)" ~?= Nothing
  , rewrite' "log(2,2)" ~?= Just 1
  , rewrite' "log(2,1)" ~?= Just 0
  , rewrite' "log(x,1)" ~?= Just 0
  , rewrite' "log(x,x)" ~?= Just 1
  , rewrite' "log(2,1+3)" ~?= Just 2
  , rewrite' "log(1+2,9)" ~?= Just 2
  , rewrite' "log(1+2,3+6)" ~?= Just 2
  , rewrite' "log(2,log(2,16))" ~?= Just 2
  , rewrite' "log(log(2,4),4)" ~?= Just 2
  , rewrite' "log(log(2,4),log(3,81))" ~?= Just 2
  , rewrite' "log(1+2,log(3,19683))" ~?= Just 2
  , rewrite' "log(log(2,4),3+1)" ~?= Just 2
  , rewrite' "log(log(log(2,4),4),4)" ~?= Just 2
  , rewrite' "log(2,log(2,log(2,65536)))" ~?= Just 2
  , rewrite' "log(log(2,log(3,40+41)),log(log(4,2^4),4*4))" ~?= Just 2 ]

--------------------------------------------------------------------------------

parserTests :: Test
parserTests = TestList
  [ parseEqn "" ([], []) ~?= Nothing
  , parseEqn "" ([], [Num (Q 1 2)]) ~?= Nothing
  , parseEqn "" ([Opr Add], [Num (Q 1 2)]) ~?= Nothing
  , parseEqn "" ([Num (Q 1 2)], []) ~?= Nothing
  , parseEqn "" ([Num (Q 1 2)], [Opr Add]) ~?= Nothing
  , parseEqn "" ([Num (Q 1 2)], [Num (Q 1 2)]) ~?= Just (Eqn (Val (Q 1 2)) (Val (Q 1 2)))
  , parseExp "" [] ~?= Nothing
  , parseExp "" [Num (Q 1 2)] ~?= Just (Val (Q 1 2))
  , parseExp "" [Chr 'x'] ~?= Just (Con 'x')
  , parseExp "x" [Chr 'x'] ~?= Just (Var 'x')
  , parseExp "" [Opr Add] ~?= Nothing
  , parseExp "" [Opr Add, Chr 'x'] ~?= Nothing
  , parseExp "" [Chr 'x', Opr Add] ~?= Nothing
  , parseExp "" [Chr 'x', Opr Add, Chr 'y'] ~?= Just (Bin Add (Con 'x') (Con 'y'))
  , parseExp "" [Fun Log] ~?= Nothing
  , parseExp "" [Fun Log, Chr 'x'] ~?= Nothing
  , parseExp "" [Fun Log, Sep] ~?= Nothing
  , parseExp "" [Fun Log, Opn] ~?= Nothing
  , parseExp "" [Fun Log, Cls] ~?= Nothing
  , parseExp "" [Fun Log, Opn, Sep] ~?= Nothing
  , parseExp "" [Fun Log, Sep, Cls] ~?= Nothing
  , parseExp "" [Fun Log, Opn, Cls] ~?= Nothing
  , parseExp "" [Fun Log, Opn, Sep, Cls] ~?= Nothing
  , parseExp "" [Fun Log, Opn, Chr 'x', Cls] ~?= Nothing
  , parseExp "" [Fun Log, Opn, Chr 'x', Sep, Cls] ~?= Nothing
  , parseExp "" [Fun Log, Opn, Sep, Chr 'x', Cls] ~?= Nothing
  , parseExp "" [Fun Log, Opn, Chr 'x', Chr 'y', Chr 'z', Cls] ~?= Nothing
  , parseExp "" [Fun Log, Opn, Sep, Chr 'x', Chr 'y', Cls] ~?= Just (App Log [Con 'x', Con 'y'])
  , parseExp "" [Fun Log, Opn, Chr 'x', Chr 'y', Sep, Cls] ~?= Just (App Log [Con 'x', Con 'y'])
  , parseExp "" [Fun Log, Chr 'x', Chr 'y'] ~?= Just (App Log [Con 'x', Con 'y'])
  , parseExp "" [Fun Log, Opn, Chr 'x', Chr 'y', Cls] ~?= Just (App Log [Con 'x', Con 'y'])
  , parseExp "" [Fun Log, Opn, Chr 'x', Sep, Chr 'y', Cls] ~?= Just (App Log [Con 'x', Con 'y'])
  , convertRPN' "(" ~?= Nothing
  , convertRPN' ")" ~?= Nothing
  , convertRPN' ")(" ~?= Nothing
  , convertRPN' "," ~?= Nothing
  , convertRPN' "(," ~?= Nothing
  , convertRPN' ",)" ~?= Nothing
  , convertRPN' "" ~?= Just []
  , convertRPN' "(())" ~?= Just []
  , convertRPN' "()()" ~?= Just [Opr Jux]
  , convertRPN' "(,,)" ~?= Just []
  , convertRPN' "1+2+3" ~?= Just [Num (Z 1), Num (Z 2), Opr Add, Num (Z 3), Opr Add]
  , convertRPN' "(1+2)+3" ~?= Just [Num (Z 1), Num (Z 2), Opr Add, Num (Z 3), Opr Add]
  , convertRPN' "1+(2+3)" ~?= Just [Num (Z 1), Num (Z 2), Num (Z 3), Opr Add, Opr Add]
  , convertRPN' "1+2*3" ~?= Just [Num (Z 1), Num (Z 2), Num (Z 3), Opr Mul, Opr Add]
  , convertRPN' "(1+2)*3" ~?= Just [Num (Z 1), Num (Z 2), Opr Add, Num (Z 3), Opr Mul]
  , convertRPN' "1+(2*3)" ~?= Just [Num (Z 1), Num (Z 2), Num (Z 3), Opr Mul, Opr Add]
  , convertRPN' "1*2+3" ~?= Just [Num (Z 1), Num (Z 2), Opr Mul, Num (Z 3), Opr Add]
  , convertRPN' "(1*2)+3" ~?= Just [Num (Z 1), Num (Z 2), Opr Mul, Num (Z 3), Opr Add]
  , convertRPN' "1*(2+3)" ~?= Just [Num (Z 1), Num (Z 2), Num (Z 3), Opr Add, Opr Mul]
  , convertRPN' "1-2-3" ~?= Just [Num (Z 1), Num (Z 2), Opr Sub, Num (Z 3), Opr Sub]
  , convertRPN' "(1-2)-3" ~?= Just [Num (Z 1), Num (Z 2), Opr Sub, Num (Z 3), Opr Sub]
  , convertRPN' "1-(2-3)" ~?= Just [Num (Z 1), Num (Z 2), Num (Z 3), Opr Sub, Opr Sub]
  , convertRPN' "1-2/3" ~?= Just [Num (Z 1), Num (Z 2), Num (Z 3), Opr Div, Opr Sub]
  , convertRPN' "(1-2)/3" ~?= Just [Num (Z 1), Num (Z 2), Opr Sub, Num (Z 3), Opr Div]
  , convertRPN' "1-(2/3)" ~?= Just [Num (Z 1), Num (Z 2), Num (Z 3), Opr Div, Opr Sub]
  , convertRPN' "1/2-3" ~?= Just [Num (Z 1), Num (Z 2), Opr Div, Num (Z 3), Opr Sub]
  , convertRPN' "(1/2)-3" ~?= Just [Num (Z 1), Num (Z 2), Opr Div, Num (Z 3), Opr Sub]
  , convertRPN' "1/(2-3)" ~?= Just [Num (Z 1), Num (Z 2), Num (Z 3), Opr Sub, Opr Div]
  , convertRPN' "xy+" ~?= Just [Chr 'x', Chr 'y', Opr Jux, Opr Add]
  , convertRPN' "x+y" ~?= Just [Chr 'x', Chr 'y', Opr Add]
  , convertRPN' "+xy" ~?= Just [Chr 'x', Chr 'y', Opr Jux, Opr Add]
  , convertRPN' "x+y+z" ~?= Just [Chr 'x', Chr 'y', Opr Add, Chr 'z', Opr Add]
  , convertRPN' "(x+y)+z" ~?= Just [Chr 'x', Chr 'y', Opr Add, Chr 'z', Opr Add]
  , convertRPN' "x+(y+z)" ~?= Just [Chr 'x', Chr 'y', Chr 'z', Opr Add, Opr Add]
  , convertRPN' "x+y*z" ~?= Just [Chr 'x', Chr 'y', Chr 'z', Opr Mul, Opr Add]
  , convertRPN' "(x+y)*z" ~?= Just [Chr 'x', Chr 'y', Opr Add, Chr 'z', Opr Mul]
  , convertRPN' "x+(y*z)" ~?= Just [Chr 'x', Chr 'y', Chr 'z', Opr Mul, Opr Add]
  , convertRPN' "x*y+z" ~?= Just [Chr 'x', Chr 'y', Opr Mul, Chr 'z', Opr Add]
  , convertRPN' "(x*y)+z" ~?= Just [Chr 'x', Chr 'y', Opr Mul, Chr 'z', Opr Add]
  , convertRPN' "x*(y+z)" ~?= Just [Chr 'x', Chr 'y', Chr 'z', Opr Add, Opr Mul]
  , convertRPN' "x-y-z" ~?= Just [Chr 'x', Chr 'y', Opr Sub, Chr 'z', Opr Sub]
  , convertRPN' "(x-y)-z" ~?= Just [Chr 'x', Chr 'y', Opr Sub, Chr 'z', Opr Sub]
  , convertRPN' "x-(y-z)" ~?= Just [Chr 'x', Chr 'y', Chr 'z', Opr Sub, Opr Sub]
  , convertRPN' "x-y/z" ~?= Just [Chr 'x', Chr 'y', Chr 'z', Opr Div, Opr Sub]
  , convertRPN' "(x-y)/z" ~?= Just [Chr 'x', Chr 'y', Opr Sub, Chr 'z', Opr Div]
  , convertRPN' "x-(y/z)" ~?= Just [Chr 'x', Chr 'y', Chr 'z', Opr Div, Opr Sub]
  , convertRPN' "x/y-z" ~?= Just [Chr 'x', Chr 'y', Opr Div, Chr 'z', Opr Sub]
  , convertRPN' "(x/y)-z" ~?= Just [Chr 'x', Chr 'y', Opr Div, Chr 'z', Opr Sub]
  , convertRPN' "x/(y-z)" ~?= Just [Chr 'x', Chr 'y', Chr 'z', Opr Sub, Opr Div]
  , convertRPN' "x^y" ~?= Just [Chr 'x', Chr 'y', Opr Exp]
  , convertRPN' "^xy" ~?= Just [Chr 'x', Chr 'y', Opr Jux, Opr Exp]
  , convertRPN' "xy^" ~?= Just [Chr 'x', Chr 'y', Opr Jux, Opr Exp]
  , convertRPN' "x^y^z" ~?= Just [Chr 'x', Chr 'y', Chr 'z', Opr Exp, Opr Exp]
  , convertRPN' "(x^y)^z" ~?= Just [Chr 'x', Chr 'y', Opr Exp, Chr 'z', Opr Exp]
  , convertRPN' "x^(y^z)" ~?= Just [Chr 'x', Chr 'y', Chr 'z', Opr Exp, Opr Exp]
  , convertRPN' "x*y^z" ~?= Just [Chr 'x', Chr 'y', Chr 'z', Opr Exp, Opr Mul]
  , convertRPN' "x*(y^z)" ~?= Just [Chr 'x', Chr 'y', Chr 'z', Opr Exp, Opr Mul]
  , convertRPN' "(x*y)^z" ~?= Just [Chr 'x', Chr 'y', Opr Mul, Chr 'z', Opr Exp]
  , convertRPN' "log" ~?= Just [Fun Log]
  , convertRPN' "log-1" ~?= Just [Fun Log, Num (Z 1), Opr Sub]
  , convertRPN' "log-1-2" ~?= Just [Fun Log, Num (Z 1), Opr Sub, Num (Z 2), Opr Sub]
  , convertRPN' "log(-1)" ~?= Just [Num (Z (-1)), Num (Z 1), Opr Jux, Fun Log]
  , convertRPN' "log(-1,-2)" ~?= Just [Num (Z (-1)), Num (Z 1), Opr Jux, Num (Z (-1)), Num (Z 2), Opr Jux, Fun Log]
  , convertRPN' "logx" ~?= Just [Fun Log, Chr 'x', Opr Jux]
  , convertRPN' "logxy" ~?= Just [Fun Log, Chr 'x', Opr Jux, Chr 'y', Opr Jux]
  , convertRPN' "xlogy" ~?= Just [Chr 'x', Fun Log, Opr Jux, Chr 'y', Opr Jux]
  , convertRPN' "xylog" ~?= Just [Chr 'x', Chr 'y', Opr Jux, Fun Log, Opr Jux]
  , convertRPN' "log(x)" ~?= Just [Chr 'x', Fun Log]
  , convertRPN' "log(x,y)" ~?= Just [Chr 'x', Chr 'y', Fun Log]
  , convertRPN' "x+logy" ~?= Just [Chr 'x', Fun Log, Chr 'y', Opr Jux, Opr Add]
  , convertRPN' "(x+log)y" ~?= Just [Chr 'x', Fun Log, Opr Add, Chr 'y', Opr Jux]
  , convertRPN' "x+log(y)" ~?= Just [Chr 'x', Chr 'y', Fun Log, Opr Add]
  , convertRPN' "logx+y" ~?= Just [Fun Log, Chr 'x', Opr Jux, Chr 'y', Opr Add]
  , convertRPN' "log(x+y)" ~?= Just [Chr 'x', Chr 'y', Opr Add, Fun Log]
  , convertRPN' "log(x)+y" ~?= Just [Chr 'x', Fun Log, Chr 'y', Opr Add]
  , convertRPN' "x+logyz" ~?= Just [Chr 'x', Fun Log, Chr 'y', Opr Jux, Chr 'z', Opr Jux, Opr Add]
  , convertRPN' "(x+log)yz" ~?= Just [Chr 'x', Fun Log, Opr Add, Chr 'y', Opr Jux, Chr 'z', Opr Jux]
  , convertRPN' "x+log(y,z)" ~?= Just [Chr 'x', Chr 'y', Chr 'z', Fun Log, Opr Add]
  , convertRPN' "logxy+z" ~?= Just [Fun Log, Chr 'x', Opr Jux, Chr 'y', Opr Jux, Chr 'z', Opr Add]
  , convertRPN' "logx(y+z)" ~?= Just [Fun Log, Chr 'x', Opr Jux, Chr 'y', Chr 'z', Opr Add, Opr Jux]
  , convertRPN' "log(x,y)+z" ~?= Just [Chr 'x', Chr 'y', Fun Log, Chr 'z', Opr Add]
  , convertAST' "" ~?= Nothing
  , convertAST' "(" ~?= Nothing
  , convertAST' ")" ~?= Nothing
  , convertAST' ")(" ~?= Nothing
  , convertAST' "()" ~?= Nothing
  , convertAST' "1" ~?= Just (Val (Z 1))
  , convertAST' "1.2" ~?= Just (Val (Q 6 5))
  , convertAST' "1 2" ~?= Just (Bin Jux (Val (Z 1)) (Val (Z 2)))
  , convertAST' "1 2 3" ~?= Just (Bin Jux (Bin Jux (Val (Z 1)) (Val (Z 2))) (Val (Z 3)))
  , convertAST' "-1" ~?= Just (Bin Jux (Val (Z (-1))) (Val (Z 1)))
  , convertAST' "-1.2" ~?= Just (Bin Jux (Val (Z (-1))) (Val (Q 6 5)))
  , convertAST' "-1 2" ~?= Just (Bin Jux (Bin Jux (Val (Z (-1))) (Val (Z 1))) (Val (Z 2)))
  , convertAST' "-1-2" ~?= Just (Bin Sub (Bin Jux (Val (Z (-1))) (Val (Z 1))) (Val (Z 2)))
  , convertAST' "-1 -2" ~?= Just (Bin Sub (Bin Jux (Val (Z (-1))) (Val (Z 1))) (Val (Z 2)))
  , convertAST' "-1.2-3" ~?= Just (Bin Sub (Bin Jux (Val (Z (-1))) (Val (Q 6 5))) (Val (Z 3)))
  , convertAST' "-1-2.3" ~?= Just (Bin Sub (Bin Jux (Val (Z (-1))) (Val (Z 1))) (Val (Q 23 10)))
  , convertAST' "x" ~?= Just (Con 'x')
  , convertAST' "x y" ~?= Just (Bin Jux (Con 'x') (Con 'y'))
  , convertAST' "x y z" ~?= Just (Bin Jux (Bin Jux (Con 'x') (Con 'y')) (Con 'z'))
  , convertAST' "-x" ~?= Just (Bin Jux (Val (Z (-1))) (Con 'x'))
  , convertAST' "- x" ~?= Just (Bin Jux (Val (Z (-1))) (Con 'x'))
  , convertAST' "-x -y" ~?= Just (Bin Sub (Bin Jux (Val (Z (-1))) (Con 'x')) (Con 'y'))
  , convertAST' "(-x)-y" ~?= Just (Bin Sub (Bin Jux (Val (Z (-1))) (Con 'x')) (Con 'y'))
  , convertAST' "-x(-y)" ~?= Just (Bin Jux (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Con 'y')))
  , convertAST' "+" ~?= Nothing
  , convertAST' "x+y" ~?= Just (Bin Add (Con 'x') (Con 'y'))
  , convertAST' "x+y+z" ~?= Just (Bin Add (Bin Add (Con 'x') (Con 'y')) (Con 'z'))
  , convertAST' "x+(y+z)" ~?= Just (Bin Add (Con 'x') (Bin Add (Con 'y') (Con 'z')))
  , convertAST' "(x+y)+z" ~?= Just (Bin Add (Bin Add (Con 'x') (Con 'y')) (Con 'z'))
  , convertAST' "-x/-y" ~?= Just (Bin Div (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Con 'y')))
  , convertAST' "-x/-y/-z" ~?= Just (Bin Div (Bin Div (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Con 'y'))) (Bin Jux (Val (Z (-1))) (Con 'z')))
  , convertAST' "-x/-(-y/-z)" ~?= Just (Bin Div (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Bin Div (Bin Jux (Val (Z (-1))) (Con 'y')) (Bin Jux (Val (Z (-1))) (Con 'z')))))
  , convertAST' "-(-x/-y)/-z" ~?= Just (Bin Div (Bin Jux (Val (Z (-1))) (Bin Div (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Con 'y')))) (Bin Jux (Val (Z (-1))) (Con 'z')))
  , convertAST' "x^y^z" ~?= Just (Bin Exp (Con 'x') (Bin Exp (Con 'y') (Con 'z')))
  , convertAST' "x^(y^z)" ~?= Just (Bin Exp (Con 'x') (Bin Exp (Con 'y') (Con 'z')))
  , convertAST' "(x^y)^z" ~?= Just (Bin Exp (Bin Exp (Con 'x') (Con 'y')) (Con 'z'))
  , convertAST' "-x^-y^-z" ~?= Just (Bin Exp (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Exp (Bin Jux (Val (Z (-1))) (Con 'y')) (Bin Jux (Val (Z (-1))) (Con 'z'))))
  , convertAST' "-x^-(-y^-z)" ~?= Just (Bin Exp (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Bin Exp (Bin Jux (Val (Z (-1))) (Con 'y')) (Bin Jux (Val (Z (-1))) (Con 'z')))))
  , convertAST' "-(-x^-y)^-z" ~?= Just (Bin Exp (Bin Jux (Val (Z (-1))) (Bin Exp (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Con 'y')))) (Bin Jux (Val (Z (-1))) (Con 'z')))
  , convertAST' "log" ~?= Nothing
  , convertAST' "logx" ~?= Nothing
  , convertAST' "xlog" ~?= Nothing
  , convertAST' "log x" ~?= Nothing
  , convertAST' "logxy" ~?= Nothing
  , convertAST' "xlogy" ~?= Nothing
  , convertAST' "xylog" ~?= Nothing
  , convertAST' "log x y" ~?= Nothing
  , convertAST' "log()" ~?= Nothing
  , convertAST' "log(x)" ~?= Nothing
  , convertAST' "log(x)y" ~?= Nothing
  , convertAST' "xlog(y)" ~?= Nothing
  , convertAST' "log(x)(y)" ~?= Nothing
  , convertAST' "log(x,y,z)" ~?= Nothing
  , convertAST' "log(x)(y)(z)" ~?= Nothing
  , convertAST' "log(x)(y,z)" ~?= Nothing
  , convertAST' "log(x,y)" ~?= Just (App Log [Con 'x', Con 'y'])
  , convertAST' "log(x,y)z" ~?= Just (Bin Jux (App Log [Con 'x', Con 'y']) (Con 'z'))
  , convertAST' "xlog(y,z)" ~?= Just (Bin Jux (Con 'x') (App Log [Con 'y', Con 'z']))
  , convertAST' "log(xy,z)" ~?= Just (App Log [Bin Jux (Con 'x') (Con 'y'), (Con 'z')])
  , convertAST' "-log(x,y)" ~?= Just (Bin Jux (Val (Z (-1))) (App Log [Con 'x', Con 'y']))
  , convertAST' "log(-x,-y)" ~?= Just (App Log [Bin Jux (Val (Z (-1))) (Con 'x'), Bin Jux (Val (Z (-1))) (Con 'y')])
  , convertAST' "log(log(x,y),z)" ~?= Just (App Log [App Log [Con 'x', Con 'y'], Con 'z'])
  , convertAST' "log(x,log(y,z))" ~?= Just (App Log [Con 'x', App Log [Con 'y', Con 'z']])
  , notPrecedes Add Sub ~?= True
  , notPrecedes Add Div ~?= True
  , notPrecedes Sub Exp ~?= True
  , notPrecedes Sub Jux ~?= True
  , notPrecedes Mul Sub ~?= False
  , notPrecedes Mul Div ~?= True
  , notPrecedes Div Exp ~?= True
  , notPrecedes Div Jux ~?= True
  , notPrecedes Exp Add ~?= False
  , notPrecedes Exp Mul ~?= False
  , notPrecedes Exp Exp ~?= False
  , notPrecedes Exp Jux ~?= True
  , notPrecedes Jux Add ~?= False
  , notPrecedes Jux Mul ~?= False
  , notPrecedes Jux Exp ~?= False
  , notPrecedes Jux Jux ~?= True
  , popArguments (-1) "" [] ~?= Nothing
  , popArguments (-1) "" [Num (Z 1)] ~?= Nothing
  , popArguments 0 "" [] ~?= Just ([], [])
  , popArguments 0 "" (map (Num . Z) [1 .. 2]) ~?= Just ([], [Num (Z 1), Num (Z 2)])
  , popArguments 2 "" [] ~?= Nothing
  , popArguments 2 "" [Num (Z 1)] ~?= Nothing
  , popArguments 2 "" (map (Num . Z) [1 .. 2]) ~?= Just ([Val (Z 1), Val (Z 2)], [])
  , popArguments 2 "" (map (Num . Z) [1 .. 4]) ~?= Just ([Val (Z 1), Val (Z 2)], [Num (Z 3), Num (Z 4)]) ]

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
  , lexEqn "x=y" ~?= Just ([Chr 'x'], [Chr 'y'])
  , lexEqn " x = y " ~?= Just ([Chr 'x'], [Chr 'y'])
  , lexEqn "-x=-y" ~?= Just ([Num (Z (-1)), Opr Jux, Chr 'x'], [Num (Z (-1)), Opr Jux, Chr 'y'])
  , lexEqn " - x = - y " ~?= Just ([Num (Z (-1)), Opr Jux, Chr 'x'], [Num (Z (-1)), Opr Jux, Chr 'y'])
  , lexExp'' "1" ~?= Just [Num (Z 1)]
  , lexExp'' "1.2" ~?= Just [Num (Q 6 5)]
  , lexExp'' "1 2" ~?= Just [Num (Z 1), Opr Jux, Num (Z 2)]
  , lexExp'' "1.2 3.4" ~?= Just [Num (Q 6 5), Opr Jux, Num (Q 17 5)]
  , lexExp'' "x" ~?= Just [Chr 'x']
  , lexExp'' "xy" ~?= Just [Chr 'x', Opr Jux, Chr 'y']
  , lexExp'' "x y" ~?= Just [Chr 'x', Opr Jux, Chr 'y']
  , lexExp'' "1.2x" ~?= Just [Num (Q 6 5), Opr Jux, Chr 'x']
  , lexExp'' "x1.2" ~?= Just [Chr 'x', Opr Jux, Num (Q 6 5)]
  , lexExp'' "+*^" ~?= Just [Opr Add, Opr Mul, Opr Exp]
  , lexExp'' "log" ~?= Just [Fun Log, Opn, Cls]
  , lexExp'' "1.2log" ~?= Just [Num (Q 6 5), Opr Jux, Fun Log, Opn, Cls]
  , lexExp'' "log1.2" ~?= Just [Fun Log, Opn, Cls, Opr Jux, Num (Q 6 5)]
  , lexExp'' "xlog" ~?= Just [Chr 'x', Opr Jux, Fun Log, Opn, Cls]
  , lexExp'' "logx" ~?= Just [Fun Log, Opn, Cls, Opr Jux, Chr 'x']
  , lexExp'' "(,)" ~?= Just [Opn, Sep, Cls]
  , lexExp'' "1.2(" ~?= Just [Num (Q 6 5), Opr Jux, Opn]
  , lexExp'' "x(" ~?= Just [Chr 'x', Opr Jux, Opn]
  , lexExp'' ")(" ~?= Just [Cls, Opr Jux, Opn]
  , lexExp'' "-2.3" ~?= Just [Num (Z (-1)), Opr Jux, Num (Q 23 10)]
  , lexExp'' "1-2.3" ~?= Just [Num (Z 1), Opr Sub, Num (Q 23 10)]
  , lexExp'' "x-2.3" ~?= Just [Chr 'x', Opr Sub, Num (Q 23 10)]
  , lexExp'' "+-2.3" ~?= Just [Opr Add, Num (Z (-1)), Opr Jux, Num (Q 23 10)]
  , lexExp'' "log-2.3" ~?= Just [Fun Log, Opn, Cls, Opr Sub, Num (Q 23 10)]
  , lexExp'' "log()-2.3" ~?= Just [Fun Log, Opn, Cls, Opr Sub, Num (Q 23 10)]
  , lexExp'' ",-2.3" ~?= Just [Sep, Num (Z (-1)), Opr Jux, Num (Q 23 10)]
  , lexExp'' "(-2.3" ~?= Just [Opn, Num (Z (-1)), Opr Jux, Num (Q 23 10)]
  , lexExp'' ")-2.3" ~?= Just [Cls, Opr Sub, Num (Q 23 10)]
  , matchNum "" ~?= Nothing
  , matchNum " " ~?= Nothing
  , matchNum "x" ~?= Nothing
  , matchNum "x1" ~?= Nothing
  , matchNum "1" ~?= Just (Num (Z 1), "")
  , matchNum "1.2" ~?= Just (Num (Q 6 5), "")
  , matchNum "1+" ~?= Just (Num (Z 1), "+")
  , matchNum "1.2+" ~?= Just (Num (Q 6 5), "+")
  , matchNum "1.2.3" ~?= Just (Num (Q 6 5), ".3")
  , matchNum " 1 .2" ~?= Just (Num (Z 1), " .2")
  , matchChr "" ~?= Nothing
  , matchChr " " ~?= Nothing
  , matchChr " x" ~?= Nothing
  , matchChr "1" ~?= Nothing
  , matchChr "1x" ~?= Nothing
  , matchChr "x" ~?= Just (Chr 'x', "")
  , matchChr "x1" ~?= Just (Chr 'x', "1")
  , matchOpr "" ~?= Nothing
  , matchOpr " " ~?= Nothing
  , matchOpr "x" ~?= Nothing
  , matchOpr "x+" ~?= Nothing
  , matchOpr " +" ~?= Nothing
  , matchOpr "+x" ~?= Just (Opr Add, "x")
  , matchOpr "++" ~?= Just (Opr Add, "+")
  , matchFun "" ~?= Nothing
  , matchFun " " ~?= Nothing
  , matchFun "x" ~?= Nothing
  , matchFun "xlog" ~?= Nothing
  , matchFun " l o g " ~?= Nothing
  , matchFun " log log " ~?= Nothing
  , matchFun "logx" ~?= Just (Fun Log, "()x")
  , matchFun "log(x)" ~?= Just (Fun Log, "(x)")
  , matchFun "loglog" ~?= Just (Fun Log, "()log")
  , matchPattern "" "" ~?= Just ""
  , matchPattern "" "x" ~?= Nothing
  , matchPattern "x" "" ~?= Just "x"
  , matchPattern "x" "x" ~?= Just ""
  , matchPattern "x" "xy" ~?= Nothing
  , matchPattern "xy" "x" ~?= Just "y"
  , matchPattern "xy" "xy" ~?= Just "" ]