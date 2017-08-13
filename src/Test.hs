module Test where

import Test.HUnit ((~?=), Test (TestList), runTestTT)

import Equation
import Lex
import Main
import Parse
import Rewrite
import Show
import Simplify

--------------------------------------------------------------------------------

lexExp'' :: Input -> Maybe Tokens
lexExp'' input = return input >>= fmap fst . lexExp

convertRPN' :: Input -> Maybe TokenQueue
convertRPN' input = return input >>= lexExp'' >>= fmap reverse . convertRPN

convertAST' :: Input -> Maybe Expression
convertAST' input = return input >>= lexExp'' >>= convertRPN >>= convertAST []

--------------------------------------------------------------------------------

analyse' :: Input -> Maybe Expression
analyse' = convertAST'

simplify' :: Input -> Maybe Expression
simplify' input = analyse' input >>= simplify

evaluate' :: Input -> Maybe Expression
evaluate' input = analyse' input >>= evaluate

applyList' :: Input -> Maybe Expression
applyList' input = fmap applyList (analyse' input)

--------------------------------------------------------------------------------

tests :: Test
tests = TestList . reverse $
  [simplifyTests, parseTests, lexTests]

--------------------------------------------------------------------------------

simplifyTests :: Test
simplifyTests = TestList
  [ simplify' "" ~?= Nothing
  , simplify' "x/x*1+2-3" ~?= return 0
  , simplify' "log(x,x)^x^x" ~?= return 1
  , simplify' "---------(-x)" ~?= return (Con 'x')
  , simplify' "-1/-(-1/-x)" ~?= return (Con 'x')
  , evaluate' "1.2+3.4" ~?= return (Val (1.2 + 3.4))
  , evaluate' "1.2-3.4" ~?= return (Val (1.2 - 3.4))
  , evaluate' "1.2*3.4" ~?= return (Val (1.2 * 3.4))
  , evaluate' "1.2/3.4" ~?= return (Val (1.2 / 3.4))
  , evaluate' "1.2^3.4" ~?= return (Val (1.2 ** 3.4))
  , evaluate' "1.2 3.4" ~?= return (Val (1.2 * 3.4))
  , evaluate' "1+x" ~?= return (1 + Con 'x')
  , evaluate' "x+1" ~?= return (Con 'x' + 1)
  , evaluate' "1+(2+3)" ~?= return 6
  , evaluate' "(1+2)+3" ~?= return 6
  , evaluate' "(1+2)+(3+4)" ~?= return 10
  , evaluate' "((1+2)+3)+4" ~?= return 10
  , evaluate' "(1+2)+x" ~?= return (3 + Con 'x')
  , evaluate' "x+(1+2)" ~?= return (Con 'x' + 3)
  , evaluate' "1+(x+2)" ~?= return (1 + (Con 'x' + 2))
  , evaluate' "(x+1)+2" ~?= return (Con 'x' + 1 + 2)
  , evaluate' "(1+2)+(x+3)" ~?= return (3 + (Con 'x' + 3))
  , evaluate' "(x+1)+(2+3)" ~?= return (Con 'x' + 1 + 5)
  , evaluate' "1+(2+(3+x))" ~?= return (1 + (2 + (3 + Con 'x')))
  , evaluate' "((x+1)+2)+3" ~?= return (Con 'x' + 1 + 2 + 3)
  , evaluate' "log(2,1+3)" ~?= return 2
  , evaluate' "log(1+2,9)" ~?= return 2
  , evaluate' "log(1+2,3+6)" ~?= return 2
  , evaluate' "log(2,log(2,16))" ~?= return 2
  , evaluate' "log(log(2,4),4)" ~?= return 2
  , evaluate' "log(log(2,4),log(3,81))" ~?= return 2
  , evaluate' "log(1+2,log(3,19683))" ~?= return 2
  , evaluate' "log(log(2,4),3+1)" ~?= return 2
  , evaluate' "log(log(log(2,4),4),4)" ~?= return 2
  , evaluate' "log(2,log(2,log(2,65536)))" ~?= return 2
  , rewriteAdd (Bin Add 0 0) ~?= return 0
  , rewriteAdd (Bin Add 0 2.3) ~?= return 2.3
  , rewriteAdd (Bin Add 2.3 0) ~?= return 2.3
  , rewriteAdd (Bin Add (-2.3) 2.3) ~?= return 0
  , rewriteAdd (Bin Add 2.3 (-2.3)) ~?= return 0
  , rewriteAdd (Bin Add 1.2 3.4) ~?= return (Bin Add 1.2 3.4)
  , rewriteAdd (Bin Add 0 (Var 'x')) ~?= return (Var 'x')
  , rewriteAdd (Bin Add (Var 'x') 0) ~?= return (Var 'x')
  , rewriteAdd (Bin Add (App Neg [Var 'x']) (Var 'x')) ~?= return 0
  , rewriteAdd (Bin Add (Var 'x') (App Neg [Var 'x'])) ~?= return 0
  , rewriteAdd (Bin Add (Var 'x') (Var 'y')) ~?= return (Bin Add (Var 'x') (Var 'y'))
  , rewriteSub (Bin Sub 0 0) ~?= return 0
  , rewriteSub (Bin Sub 0 2.3) ~?= return (-2.3)
  , rewriteSub (Bin Sub 2.3 0) ~?= return 2.3
  , rewriteSub (Bin Sub 2.3 2.3) ~?= return 0
  , rewriteSub (Bin Sub 1.2 3.4) ~?= return (Bin Add 1.2 (negate 3.4))
  , rewriteSub (Bin Sub 0 (Var 'x')) ~?= return (-Var 'x')
  , rewriteSub (Bin Sub (Var 'x') 0) ~?= return (Var 'x')
  , rewriteSub (Bin Sub (Var 'x') (Var 'x')) ~?= return 0
  , rewriteSub (Bin Sub (Var 'x') (Var 'y')) ~?= return (Bin Add (Var 'x') (-Var 'y'))
  , rewriteMul (Bin Mul 0 0) ~?= return 0
  , rewriteMul (Bin Mul 0 2.3) ~?= return 0
  , rewriteMul (Bin Mul 2.3 0) ~?= return 0
  , rewriteMul (Bin Mul 1 2.3) ~?= return 2.3
  , rewriteMul (Bin Mul 2.3 1) ~?= return 2.3
  , rewriteMul (Bin Mul (Val (-1)) 2.3) ~?= return (-2.3)
  , rewriteMul (Bin Mul 2.3 (Val (-1))) ~?= return (-2.3)
  , rewriteMul (Bin Mul (recip 2.3) 2.3) ~?= return 1
  , rewriteMul (Bin Mul 2.3 (recip 2.3)) ~?= return 1
  , rewriteMul (Bin Mul (-recip 2.3) 2.3) ~?= return (Val (-1))
  , rewriteMul (Bin Mul (recip (-2.3)) 2.3) ~?= return (Val (-1))
  , rewriteMul (Bin Mul 2.3 (-recip 2.3)) ~?= return (Val (-1))
  , rewriteMul (Bin Mul 2.3 (recip (-2.3))) ~?= return (Val (-1))
  , rewriteMul (Bin Mul 1.2 3.4) ~?= return (Bin Mul 1.2 3.4)
  , rewriteMul (Bin Mul 0 (Var 'x')) ~?= return 0
  , rewriteMul (Bin Mul (Var 'x') 0) ~?= return 0
  , rewriteMul (Bin Mul 1 (Var 'x')) ~?= return (Var 'x')
  , rewriteMul (Bin Mul (Var 'x') 1) ~?= return (Var 'x')
  , rewriteMul (Bin Mul (Val (-1)) (Var 'x')) ~?= return (-Var 'x')
  , rewriteMul (Bin Mul (Var 'x') (Val (-1))) ~?= return (-Var 'x')
  , rewriteMul (Bin Mul (recip (Var 'x')) (Var 'x')) ~?= return 1
  , rewriteMul (Bin Mul (Var 'x') (recip (Var 'x'))) ~?= return 1
  , rewriteMul (Bin Mul (-recip (Var 'x')) (Var 'x')) ~?= return (Val (-1))
  , rewriteMul (Bin Mul (recip (-Var 'x')) (Var 'x')) ~?= return (Val (-1))
  , rewriteMul (Bin Mul (Var 'x') (-recip (Var 'x'))) ~?= return (Val (-1))
  , rewriteMul (Bin Mul (Var 'x') (recip (-Var 'x'))) ~?= return (Val (-1))
  , rewriteMul (Bin Mul (Var 'x') (Var 'y')) ~?= return (Bin Mul (Var 'x') (Var 'y'))
  , rewriteDiv (Bin Div 0 0) ~?= Nothing
  , rewriteDiv (Bin Div 0 2.3) ~?= return 0
  , rewriteDiv (Bin Div 2.3 0) ~?= Nothing
  , rewriteDiv (Bin Div 1 2.3) ~?= return (recip 2.3)
  , rewriteDiv (Bin Div 2.3 1) ~?= return 2.3
  , rewriteDiv (Bin Div 2.3 2.3) ~?= return 1
  , rewriteDiv (Bin Div (Val (-1)) 2.3) ~?= return (-recip 2.3)
  , rewriteDiv (Bin Div 2.3 (Val (-1))) ~?= return (-2.3)
  , rewriteDiv (Bin Div (-2.3) 2.3) ~?= return (Val (-1))
  , rewriteDiv (Bin Div 2.3 (-2.3)) ~?= return (Val (-1))
  , rewriteDiv (Bin Div 1.2 3.4) ~?= return (Bin Mul 1.2 (recip 3.4))
  , rewriteDiv (Bin Div 0 (Var 'x')) ~?= return 0
  , rewriteDiv (Bin Div (Var 'x') 0) ~?= Nothing
  , rewriteDiv (Bin Div 1 (Var 'x')) ~?= return (recip (Var 'x'))
  , rewriteDiv (Bin Div (Var 'x') 1) ~?= return (Var 'x')
  , rewriteDiv (Bin Div (Var 'x') (Var 'x')) ~?= return 1
  , rewriteDiv (Bin Div (Val (-1)) (Var 'x')) ~?= return (-recip (Var 'x'))
  , rewriteDiv (Bin Div (Var 'x') (Val (-1))) ~?= return (-Var 'x')
  , rewriteDiv (Bin Div (-Var 'x') (Var 'x')) ~?= return (Val (-1))
  , rewriteDiv (Bin Div (Var 'x') (-Var 'x')) ~?= return (Val (-1))
  , rewriteDiv (Bin Div (Var 'x') (Var 'y')) ~?= return (Bin Mul (Var 'x') (recip (Var 'y')))
  , rewriteExp (Bin Exp 0 0) ~?= Nothing
  , rewriteExp (Bin Exp (Val (-2.3)) 0) ~?= Nothing
  , rewriteExp (Bin Exp 0 2.3) ~?= return 0
  , rewriteExp (Bin Exp 2.3 0) ~?= return 1
  , rewriteExp (Bin Exp (Val (-2.3)) 1) ~?= Nothing
  , rewriteExp (Bin Exp 1 2.3) ~?= return 1
  , rewriteExp (Bin Exp 2.3 1) ~?= return 2.3
  , rewriteExp (Bin Exp (Val (-2.3)) (Val (-1))) ~?= Nothing
  , rewriteExp (Bin Exp (Val (-1)) 2.3) ~?= Nothing
  , rewriteExp (Bin Exp 2.3 (Val (-1))) ~?= return (recip 2.3)
  , rewriteExp (Bin Exp (Val (-1.2)) 3.4) ~?= Nothing
  , rewriteExp (Bin Exp 1.2 3.4) ~?= return (Bin Exp 1.2 3.4)
  , rewriteExp (Bin Exp 0 (Var 'x')) ~?= return 0
  , rewriteExp (Bin Exp (Var 'x') 0) ~?= return 1
  , rewriteExp (Bin Exp 1 (Var 'x')) ~?= return 1
  , rewriteExp (Bin Exp (Var 'x') 1) ~?= return (Var 'x')
  , rewriteExp (Bin Exp (Var 'x') (Val (-1))) ~?= return (recip (Var 'x'))
  , rewriteExp (Bin Exp (Val (-2.3)) (Var 'x')) ~?= Nothing
  , rewriteExp (Bin Exp (Var 'x') (Var 'x')) ~?= return (Bin Exp (Var 'x') (Var 'x'))
  , rewriteMul (Bin Jux 0 0) ~?= return 0
  , rewriteMul (Bin Jux 0 2.3) ~?= return 0
  , rewriteMul (Bin Jux 2.3 0) ~?= return 0
  , rewriteMul (Bin Jux 1 2.3) ~?= return 2.3
  , rewriteMul (Bin Jux 2.3 1) ~?= return 2.3
  , rewriteMul (Bin Jux (Val (-1)) 2.3) ~?= return (-2.3)
  , rewriteMul (Bin Jux 2.3 (Val (-1))) ~?= return (-2.3)
  , rewriteMul (Bin Jux (recip 2.3) 2.3) ~?= return 1
  , rewriteMul (Bin Jux 2.3 (recip 2.3)) ~?= return 1
  , rewriteMul (Bin Mul (-recip 2.3) 2.3) ~?= return (Val (-1))
  , rewriteMul (Bin Jux (recip (-2.3)) 2.3) ~?= return (Val (-1))
  , rewriteMul (Bin Jux 2.3 (-recip 2.3)) ~?= return (Val (-1))
  , rewriteMul (Bin Jux 2.3 (recip (-2.3))) ~?= return (Val (-1))
  , rewriteMul (Bin Jux 1.2 3.4) ~?= return (Bin Jux 1.2 3.4)
  , rewriteMul (Bin Jux 0 (Var 'x')) ~?= return 0
  , rewriteMul (Bin Jux (Var 'x') 0) ~?= return 0
  , rewriteMul (Bin Jux 1 (Var 'x')) ~?= return (Var 'x')
  , rewriteMul (Bin Jux (Var 'x') 1) ~?= return (Var 'x')
  , rewriteMul (Bin Jux (Val (-1)) (Var 'x')) ~?= return (-Var 'x')
  , rewriteMul (Bin Jux (Var 'x') (Val (-1))) ~?= return (-Var 'x')
  , rewriteMul (Bin Jux (recip (Var 'x')) (Var 'x')) ~?= return 1
  , rewriteMul (Bin Jux (Var 'x') (recip (Var 'x'))) ~?= return 1
  , rewriteMul (Bin Jux (-recip (Var 'x')) (Var 'x')) ~?= return (Val (-1))
  , rewriteMul (Bin Jux (recip (-Var 'x')) (Var 'x')) ~?= return (Val (-1))
  , rewriteMul (Bin Jux (Var 'x') (-recip (Var 'x'))) ~?= return (Val (-1))
  , rewriteMul (Bin Jux (Var 'x') (recip (-Var 'x'))) ~?= return (Val (-1))
  , rewriteMul (Bin Jux (Var 'x') (Var 'y')) ~?= return (Bin Jux (Var 'x') (Var 'y'))
  , rewriteLog (App Log [0, 0]) ~?= Nothing
  , rewriteLog (App Log [0, 4]) ~?= Nothing
  , rewriteLog (App Log [2, 0]) ~?= Nothing
  , rewriteLog (App Log [0, Var 'x']) ~?= Nothing
  , rewriteLog (App Log [Var 'x', 0]) ~?= Nothing
  , rewriteLog (App Log [Val (-1), Val (-1)]) ~?= Nothing
  , rewriteLog (App Log [0, Val (-1)]) ~?= Nothing
  , rewriteLog (App Log [Val (-1), 0]) ~?= Nothing
  , rewriteLog (App Log [2, Val (-1)]) ~?= Nothing
  , rewriteLog (App Log [Val (-1), 4]) ~?= Nothing
  , rewriteLog (App Log [Var 'x', Val (-1)]) ~?= Nothing
  , rewriteLog (App Log [Val (-1), Var 'x']) ~?= Nothing
  , rewriteLog (App Log [1, 1]) ~?= Nothing
  , rewriteLog (App Log [0, 1]) ~?= Nothing
  , rewriteLog (App Log [1, 0]) ~?= Nothing
  , rewriteLog (App Log [Val (-1), 1]) ~?= Nothing
  , rewriteLog (App Log [1, Val (-1)]) ~?= Nothing
  , rewriteLog (App Log [1, 4]) ~?= Nothing
  , rewriteLog (App Log [1, Var 'x']) ~?= Nothing
  , rewriteLog (App Log [2, 2]) ~?= return 1
  , rewriteLog (App Log [2, 1]) ~?= return 0
  , rewriteLog (App Log [Var 'x', 1]) ~?= return 0
  , rewriteLog (App Log [Var 'x', Var 'x']) ~?= return 1
  , applyList' "1+2" ~?= return (1 + 2)
  , applyList' "1+2+3" ~?= return (App Sum [1, 2, 3])
  , applyList' "1+2+3+4" ~?= return (App Sum [1, 2, 3, 4])
  , applyList' "1+2+3+4+5" ~?= return (App Sum [1, 2, 3, 4, 5])
  , applyList' "1+((2+(3+4))+5)" ~?= return (App Sum [1, 2, 3, 4, 5])
  , applyList' "1*2" ~?= return (1 * 2)
  , applyList' "1*2*3" ~?= return (App Prd [1, 2, 3])
  , applyList' "1*2*3*4" ~?= return (App Prd [1, 2, 3, 4])
  , applyList' "1*2*3*4*5" ~?= return (App Prd [1, 2, 3, 4, 5])
  , applyList' "1*((2*(3*4))*5)" ~?= return (App Prd [1, 2, 3, 4, 5])
  , applyList' "1+2*3" ~?= return (1 + 2 * 3)
  , applyList' "1*2+3" ~?= return (1 * 2 + 3)
  , applyList' "1+2+3*4" ~?= return (App Sum [1, 2, 3 * 4])
  , applyList' "1+2*3+4" ~?= return (App Sum [1, 4, 2 * 3])
  , applyList' "1*2+3+4" ~?= return (App Sum [3, 4, 1 * 2])
  , applyList' "1+2*3*4" ~?= return (1 + App Prd [2, 3, 4])
  , applyList' "1*2+3*4" ~?= return (1 * 2 + 3 * 4)
  , applyList' "1*2*3+4" ~?= return (App Prd [1, 2, 3] + 4)
  , applyList' "1+2+3+4*5" ~?= return (App Sum [1, 2, 3, 4 * 5])
  , applyList' "1+2+3*4+5" ~?= return (App Sum [1, 2, 5, 3 * 4])
  , applyList' "1+2*3+4+5" ~?= return (App Sum [1, 4, 5, 2 * 3])
  , applyList' "1*2+3+4+5" ~?= return (App Sum [3, 4, 5, 1 * 2])
  , applyList' "1+2+3*4*5" ~?= return (App Sum [1, 2, App Prd [3, 4, 5]])
  , applyList' "1+2*3+4*5" ~?= return (App Sum [1, 2 * 3, 4 * 5])
  , applyList' "1*2+3+4*5" ~?= return (App Sum [3, 1 * 2, 4 * 5])
  , applyList' "1+2*3*4+5" ~?= return (App Sum [1, 5, App Prd [2, 3, 4]])
  , applyList' "1*2+3*4+5" ~?= return (App Sum [5, 1 * 2, 3 * 4])
  , applyList' "1*2*3+4+5" ~?= return (App Sum [4, 5, App Prd [1, 2, 3]]) ]

--------------------------------------------------------------------------------

parseTests :: Test
parseTests = TestList
  [ parseEqn "" ([], []) ~?= Nothing
  , parseEqn "" ([], [Num (Q 1 2)]) ~?= Nothing
  , parseEqn "" ([Opr Add], [Num (Q 1 2)]) ~?= Nothing
  , parseEqn "" ([Num (Q 1 2)], []) ~?= Nothing
  , parseEqn "" ([Num (Q 1 2)], [Opr Add]) ~?= Nothing
  , parseEqn "" ([Num (Q 1 2)], [Num (Q 1 2)]) ~?= return (Eqn (Val (Q 1 2)) (Val (Q 1 2)))
  , parseExp "" [] ~?= Nothing
  , parseExp "" [Num (Q 1 2)] ~?= return (Val (Q 1 2))
  , parseExp "" [Chr 'x'] ~?= return (Con 'x')
  , parseExp "x" [Chr 'x'] ~?= return (Var 'x')
  , parseExp "" [Opr Add] ~?= Nothing
  , parseExp "" [Opr Add, Chr 'x'] ~?= Nothing
  , parseExp "" [Chr 'x', Opr Add] ~?= Nothing
  , parseExp "" [Chr 'x', Opr Add, Chr 'y'] ~?= return (Bin Add (Con 'x') (Con 'y'))
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
  , parseExp "" [Fun Log, Opn, Sep, Chr 'x', Chr 'y', Cls] ~?= return (App Log [Con 'x', Con 'y'])
  , parseExp "" [Fun Log, Opn, Chr 'x', Chr 'y', Sep, Cls] ~?= return (App Log [Con 'x', Con 'y'])
  , parseExp "" [Fun Log, Chr 'x', Chr 'y'] ~?= return (App Log [Con 'x', Con 'y'])
  , parseExp "" [Fun Log, Opn, Chr 'x', Chr 'y', Cls] ~?= return (App Log [Con 'x', Con 'y'])
  , parseExp "" [Fun Log, Opn, Chr 'x', Sep, Chr 'y', Cls] ~?= return (App Log [Con 'x', Con 'y'])
  , convertRPN' "(" ~?= Nothing
  , convertRPN' ")" ~?= Nothing
  , convertRPN' ")(" ~?= Nothing
  , convertRPN' "," ~?= Nothing
  , convertRPN' "(," ~?= Nothing
  , convertRPN' ",)" ~?= Nothing
  , convertRPN' "" ~?= return []
  , convertRPN' "(())" ~?= return []
  , convertRPN' "()()" ~?= return [Opr Mul]
  , convertRPN' "(,,)" ~?= return []
  , convertRPN' "1+2+3" ~?= return [Num (Z 1), Num (Z 2), Opr Add, Num (Z 3), Opr Add]
  , convertRPN' "(1+2)+3" ~?= return [Num (Z 1), Num (Z 2), Opr Add, Num (Z 3), Opr Add]
  , convertRPN' "1+(2+3)" ~?= return [Num (Z 1), Num (Z 2), Num (Z 3), Opr Add, Opr Add]
  , convertRPN' "1+2*3" ~?= return [Num (Z 1), Num (Z 2), Num (Z 3), Opr Mul, Opr Add]
  , convertRPN' "(1+2)*3" ~?= return [Num (Z 1), Num (Z 2), Opr Add, Num (Z 3), Opr Mul]
  , convertRPN' "1+(2*3)" ~?= return [Num (Z 1), Num (Z 2), Num (Z 3), Opr Mul, Opr Add]
  , convertRPN' "1*2+3" ~?= return [Num (Z 1), Num (Z 2), Opr Mul, Num (Z 3), Opr Add]
  , convertRPN' "(1*2)+3" ~?= return [Num (Z 1), Num (Z 2), Opr Mul, Num (Z 3), Opr Add]
  , convertRPN' "1*(2+3)" ~?= return [Num (Z 1), Num (Z 2), Num (Z 3), Opr Add, Opr Mul]
  , convertRPN' "1-2-3" ~?= return [Num (Z 1), Num (Z 2), Opr Sub, Num (Z 3), Opr Sub]
  , convertRPN' "(1-2)-3" ~?= return [Num (Z 1), Num (Z 2), Opr Sub, Num (Z 3), Opr Sub]
  , convertRPN' "1-(2-3)" ~?= return [Num (Z 1), Num (Z 2), Num (Z 3), Opr Sub, Opr Sub]
  , convertRPN' "1-2/3" ~?= return [Num (Z 1), Num (Z 2), Num (Z 3), Opr Div, Opr Sub]
  , convertRPN' "(1-2)/3" ~?= return [Num (Z 1), Num (Z 2), Opr Sub, Num (Z 3), Opr Div]
  , convertRPN' "1-(2/3)" ~?= return [Num (Z 1), Num (Z 2), Num (Z 3), Opr Div, Opr Sub]
  , convertRPN' "1/2-3" ~?= return [Num (Z 1), Num (Z 2), Opr Div, Num (Z 3), Opr Sub]
  , convertRPN' "(1/2)-3" ~?= return [Num (Z 1), Num (Z 2), Opr Div, Num (Z 3), Opr Sub]
  , convertRPN' "1/(2-3)" ~?= return [Num (Z 1), Num (Z 2), Num (Z 3), Opr Sub, Opr Div]
  , convertRPN' "xy+" ~?= return [Chr 'x', Chr 'y', Opr Mul, Opr Add]
  , convertRPN' "x+y" ~?= return [Chr 'x', Chr 'y', Opr Add]
  , convertRPN' "+xy" ~?= return [Chr 'x', Chr 'y', Opr Mul, Opr Add]
  , convertRPN' "x+y+z" ~?= return [Chr 'x', Chr 'y', Opr Add, Chr 'z', Opr Add]
  , convertRPN' "(x+y)+z" ~?= return [Chr 'x', Chr 'y', Opr Add, Chr 'z', Opr Add]
  , convertRPN' "x+(y+z)" ~?= return [Chr 'x', Chr 'y', Chr 'z', Opr Add, Opr Add]
  , convertRPN' "x+y*z" ~?= return [Chr 'x', Chr 'y', Chr 'z', Opr Mul, Opr Add]
  , convertRPN' "(x+y)*z" ~?= return [Chr 'x', Chr 'y', Opr Add, Chr 'z', Opr Mul]
  , convertRPN' "x+(y*z)" ~?= return [Chr 'x', Chr 'y', Chr 'z', Opr Mul, Opr Add]
  , convertRPN' "x*y+z" ~?= return [Chr 'x', Chr 'y', Opr Mul, Chr 'z', Opr Add]
  , convertRPN' "(x*y)+z" ~?= return [Chr 'x', Chr 'y', Opr Mul, Chr 'z', Opr Add]
  , convertRPN' "x*(y+z)" ~?= return [Chr 'x', Chr 'y', Chr 'z', Opr Add, Opr Mul]
  , convertRPN' "x-y-z" ~?= return [Chr 'x', Chr 'y', Opr Sub, Chr 'z', Opr Sub]
  , convertRPN' "(x-y)-z" ~?= return [Chr 'x', Chr 'y', Opr Sub, Chr 'z', Opr Sub]
  , convertRPN' "x-(y-z)" ~?= return [Chr 'x', Chr 'y', Chr 'z', Opr Sub, Opr Sub]
  , convertRPN' "x-y/z" ~?= return [Chr 'x', Chr 'y', Chr 'z', Opr Div, Opr Sub]
  , convertRPN' "(x-y)/z" ~?= return [Chr 'x', Chr 'y', Opr Sub, Chr 'z', Opr Div]
  , convertRPN' "x-(y/z)" ~?= return [Chr 'x', Chr 'y', Chr 'z', Opr Div, Opr Sub]
  , convertRPN' "x/y-z" ~?= return [Chr 'x', Chr 'y', Opr Div, Chr 'z', Opr Sub]
  , convertRPN' "(x/y)-z" ~?= return [Chr 'x', Chr 'y', Opr Div, Chr 'z', Opr Sub]
  , convertRPN' "x/(y-z)" ~?= return [Chr 'x', Chr 'y', Chr 'z', Opr Sub, Opr Div]
  , convertRPN' "x^y" ~?= return [Chr 'x', Chr 'y', Opr Exp]
  , convertRPN' "^xy" ~?= return [Chr 'x', Opr Exp, Chr 'y', Opr Mul]
  , convertRPN' "xy^" ~?= return [Chr 'x', Chr 'y', Opr Exp, Opr Mul]
  , convertRPN' "x^y^z" ~?= return [Chr 'x', Chr 'y', Chr 'z', Opr Exp, Opr Exp]
  , convertRPN' "(x^y)^z" ~?= return [Chr 'x', Chr 'y', Opr Exp, Chr 'z', Opr Exp]
  , convertRPN' "x^(y^z)" ~?= return [Chr 'x', Chr 'y', Chr 'z', Opr Exp, Opr Exp]
  , convertRPN' "x*y^z" ~?= return [Chr 'x', Chr 'y', Chr 'z', Opr Exp, Opr Mul]
  , convertRPN' "x*(y^z)" ~?= return [Chr 'x', Chr 'y', Chr 'z', Opr Exp, Opr Mul]
  , convertRPN' "(x*y)^z" ~?= return [Chr 'x', Chr 'y', Opr Mul, Chr 'z', Opr Exp]
  , convertRPN' "log" ~?= return [Fun Log]
  , convertRPN' "log-1" ~?= return [Fun Log, Num (Z 1), Opr Sub]
  , convertRPN' "log-1-2" ~?= return [Fun Log, Num (Z 1), Opr Sub, Num (Z 2), Opr Sub]
  , convertRPN' "log(-1)" ~?= return [Num (Z (-1)), Num (Z 1), Opr Jux, Fun Log]
  , convertRPN' "log(-1,-2)" ~?= return [Num (Z (-1)), Num (Z 1), Opr Jux, Num (Z (-1)), Num (Z 2), Opr Jux, Fun Log]
  , convertRPN' "logx" ~?= return [Fun Log, Chr 'x', Opr Mul]
  , convertRPN' "logxy" ~?= return [Fun Log, Chr 'x', Opr Mul, Chr 'y', Opr Mul]
  , convertRPN' "xlogy" ~?= return [Chr 'x', Fun Log, Opr Mul, Chr 'y', Opr Mul]
  , convertRPN' "xylog" ~?= return [Chr 'x', Chr 'y', Opr Mul, Fun Log, Opr Mul]
  , convertRPN' "log(x)" ~?= return [Chr 'x', Fun Log]
  , convertRPN' "log(x,y)" ~?= return [Chr 'x', Chr 'y', Fun Log]
  , convertRPN' "x+logy" ~?= return [Chr 'x', Fun Log, Chr 'y', Opr Mul, Opr Add]
  , convertRPN' "(x+log)y" ~?= return [Chr 'x', Fun Log, Opr Add, Chr 'y', Opr Mul]
  , convertRPN' "x+log(y)" ~?= return [Chr 'x', Chr 'y', Fun Log, Opr Add]
  , convertRPN' "logx+y" ~?= return [Fun Log, Chr 'x', Opr Mul, Chr 'y', Opr Add]
  , convertRPN' "log(x+y)" ~?= return [Chr 'x', Chr 'y', Opr Add, Fun Log]
  , convertRPN' "log(x)+y" ~?= return [Chr 'x', Fun Log, Chr 'y', Opr Add]
  , convertRPN' "x+logyz" ~?= return [Chr 'x', Fun Log, Chr 'y', Opr Mul, Chr 'z', Opr Mul, Opr Add]
  , convertRPN' "(x+log)yz" ~?= return [Chr 'x', Fun Log, Opr Add, Chr 'y', Opr Mul, Chr 'z', Opr Mul]
  , convertRPN' "x+log(y,z)" ~?= return [Chr 'x', Chr 'y', Chr 'z', Fun Log, Opr Add]
  , convertRPN' "logxy+z" ~?= return [Fun Log, Chr 'x', Opr Mul, Chr 'y', Opr Mul, Chr 'z', Opr Add]
  , convertRPN' "logx(y+z)" ~?= return [Fun Log, Chr 'x', Opr Mul, Chr 'y', Chr 'z', Opr Add, Opr Mul]
  , convertRPN' "log(x,y)+z" ~?= return [Chr 'x', Chr 'y', Fun Log, Chr 'z', Opr Add]
  , convertAST' "" ~?= Nothing
  , convertAST' "(" ~?= Nothing
  , convertAST' ")" ~?= Nothing
  , convertAST' ")(" ~?= Nothing
  , convertAST' "()" ~?= Nothing
  , convertAST' "1" ~?= return (Val (Z 1))
  , convertAST' "1.2" ~?= return (Val (Q 6 5))
  , convertAST' "1 2" ~?= return (Bin Mul (Val (Z 1)) (Val (Z 2)))
  , convertAST' "1 2 3" ~?= return (Bin Mul (Bin Mul (Val (Z 1)) (Val (Z 2))) (Val (Z 3)))
  , convertAST' "-1" ~?= return (Bin Jux (Val (Z (-1))) (Val (Z 1)))
  , convertAST' "-1.2" ~?= return (Bin Jux (Val (Z (-1))) (Val (Q 6 5)))
  , convertAST' "-1 2" ~?= return (Bin Mul (Bin Jux (Val (Z (-1))) (Val (Z 1))) (Val (Z 2)))
  , convertAST' "-1-2" ~?= return (Bin Sub (Bin Jux (Val (Z (-1))) (Val (Z 1))) (Val (Z 2)))
  , convertAST' "-1 -2" ~?= return (Bin Sub (Bin Jux (Val (Z (-1))) (Val (Z 1))) (Val (Z 2)))
  , convertAST' "-1.2-3" ~?= return (Bin Sub (Bin Jux (Val (Z (-1))) (Val (Q 6 5))) (Val (Z 3)))
  , convertAST' "-1-2.3" ~?= return (Bin Sub (Bin Jux (Val (Z (-1))) (Val (Z 1))) (Val (Q 23 10)))
  , convertAST' "x" ~?= return (Con 'x')
  , convertAST' "x y" ~?= return (Bin Mul (Con 'x') (Con 'y'))
  , convertAST' "x y z" ~?= return (Bin Mul (Bin Mul (Con 'x') (Con 'y')) (Con 'z'))
  , convertAST' "-x" ~?= return (Bin Jux (Val (Z (-1))) (Con 'x'))
  , convertAST' "- x" ~?= return (Bin Jux (Val (Z (-1))) (Con 'x'))
  , convertAST' "-x -y" ~?= return (Bin Sub (Bin Jux (Val (Z (-1))) (Con 'x')) (Con 'y'))
  , convertAST' "(-x)-y" ~?= return (Bin Sub (Bin Jux (Val (Z (-1))) (Con 'x')) (Con 'y'))
  , convertAST' "-x(-y)" ~?= return (Bin Mul (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Con 'y')))
  , convertAST' "+" ~?= Nothing
  , convertAST' "x+y" ~?= return (Bin Add (Con 'x') (Con 'y'))
  , convertAST' "x+y+z" ~?= return (Bin Add (Bin Add (Con 'x') (Con 'y')) (Con 'z'))
  , convertAST' "x+(y+z)" ~?= return (Bin Add (Con 'x') (Bin Add (Con 'y') (Con 'z')))
  , convertAST' "(x+y)+z" ~?= return (Bin Add (Bin Add (Con 'x') (Con 'y')) (Con 'z'))
  , convertAST' "-x/-y" ~?= return (Bin Div (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Con 'y')))
  , convertAST' "-x/-y/-z" ~?= return (Bin Div (Bin Div (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Con 'y'))) (Bin Jux (Val (Z (-1))) (Con 'z')))
  , convertAST' "-x/-(-y/-z)" ~?= return (Bin Div (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Bin Div (Bin Jux (Val (Z (-1))) (Con 'y')) (Bin Jux (Val (Z (-1))) (Con 'z')))))
  , convertAST' "-(-x/-y)/-z" ~?= return (Bin Div (Bin Jux (Val (Z (-1))) (Bin Div (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Con 'y')))) (Bin Jux (Val (Z (-1))) (Con 'z')))
  , convertAST' "x^y^z" ~?= return (Bin Exp (Con 'x') (Bin Exp (Con 'y') (Con 'z')))
  , convertAST' "x^(y^z)" ~?= return (Bin Exp (Con 'x') (Bin Exp (Con 'y') (Con 'z')))
  , convertAST' "(x^y)^z" ~?= return (Bin Exp (Bin Exp (Con 'x') (Con 'y')) (Con 'z'))
  , convertAST' "-x^-y^-z" ~?= return (Bin Exp (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Exp (Bin Jux (Val (Z (-1))) (Con 'y')) (Bin Jux (Val (Z (-1))) (Con 'z'))))
  , convertAST' "-x^-(-y^-z)" ~?= return (Bin Exp (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Bin Exp (Bin Jux (Val (Z (-1))) (Con 'y')) (Bin Jux (Val (Z (-1))) (Con 'z')))))
  , convertAST' "-(-x^-y)^-z" ~?= return (Bin Exp (Bin Jux (Val (Z (-1))) (Bin Exp (Bin Jux (Val (Z (-1))) (Con 'x')) (Bin Jux (Val (Z (-1))) (Con 'y')))) (Bin Jux (Val (Z (-1))) (Con 'z')))
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
  , convertAST' "log(x,y)" ~?= return (App Log [Con 'x', Con 'y'])
  , convertAST' "log(x,y)z" ~?= return (Bin Mul (App Log [Con 'x', Con 'y']) (Con 'z'))
  , convertAST' "xlog(y,z)" ~?= return (Bin Mul (Con 'x') (App Log [Con 'y', Con 'z']))
  , convertAST' "log(xy,z)" ~?= return (App Log [Bin Mul (Con 'x') (Con 'y'), (Con 'z')])
  , convertAST' "-log(x,y)" ~?= return (Bin Jux (Val (Z (-1))) (App Log [Con 'x', Con 'y']))
  , convertAST' "log(-x,-y)" ~?= return (App Log [Bin Jux (Val (Z (-1))) (Con 'x'), Bin Jux (Val (Z (-1))) (Con 'y')])
  , convertAST' "log(log(x,y),z)" ~?= return (App Log [App Log [Con 'x', Con 'y'], Con 'z'])
  , convertAST' "log(x,log(y,z))" ~?= return (App Log [Con 'x', App Log [Con 'y', Con 'z']])
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
  , popArguments 0 "" [] ~?= return ([], [])
  , popArguments 0 "" (map (Num . Z) [1 .. 2]) ~?= return ([], [Num (Z 1), Num (Z 2)])
  , popArguments 2 "" [] ~?= Nothing
  , popArguments 2 "" [Num (Z 1)] ~?= Nothing
  , popArguments 2 "" (map (Num . Z) [1 .. 2]) ~?= return ([Val (Z 1), Val (Z 2)], [])
  , popArguments 2 "" (map (Num . Z) [1 .. 4]) ~?= return ([Val (Z 1), Val (Z 2)], [Num (Z 3), Num (Z 4)]) ]

--------------------------------------------------------------------------------

lexTests :: Test
lexTests = TestList
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
  , lexEqn "x=y" ~?= return ([Chr 'x'], [Chr 'y'])
  , lexEqn " x = y " ~?= return ([Chr 'x'], [Chr 'y'])
  , lexEqn "-x=-y" ~?= return ([Num (Z (-1)), Opr Jux, Chr 'x'], [Num (Z (-1)), Opr Jux, Chr 'y'])
  , lexEqn " - x = - y " ~?= return ([Num (Z (-1)), Opr Jux, Chr 'x'], [Num (Z (-1)), Opr Jux, Chr 'y'])
  , lexExp'' "1" ~?= return [Num (Z 1)]
  , lexExp'' "1.2" ~?= return [Num (Q 6 5)]
  , lexExp'' "1 2" ~?= return [Num (Z 1), Opr Mul, Num (Z 2)]
  , lexExp'' "1.2 3.4" ~?= return [Num (Q 6 5), Opr Mul, Num (Q 17 5)]
  , lexExp'' "x" ~?= return [Chr 'x']
  , lexExp'' "xy" ~?= return [Chr 'x', Opr Mul, Chr 'y']
  , lexExp'' "x y" ~?= return [Chr 'x', Opr Mul, Chr 'y']
  , lexExp'' "1.2x" ~?= return [Num (Q 6 5), Opr Mul, Chr 'x']
  , lexExp'' "x1.2" ~?= return [Chr 'x', Opr Mul, Num (Q 6 5)]
  , lexExp'' "+*^" ~?= return [Opr Add, Opr Mul, Opr Exp]
  , lexExp'' "log" ~?= return [Fun Log, Opn, Cls]
  , lexExp'' "1.2log" ~?= return [Num (Q 6 5), Opr Mul, Fun Log, Opn, Cls]
  , lexExp'' "log1.2" ~?= return [Fun Log, Opn, Cls, Opr Mul, Num (Q 6 5)]
  , lexExp'' "xlog" ~?= return [Chr 'x', Opr Mul, Fun Log, Opn, Cls]
  , lexExp'' "logx" ~?= return [Fun Log, Opn, Cls, Opr Mul, Chr 'x']
  , lexExp'' "(,)" ~?= return [Opn, Sep, Cls]
  , lexExp'' "1.2(" ~?= return [Num (Q 6 5), Opr Mul, Opn]
  , lexExp'' "x(" ~?= return [Chr 'x', Opr Mul, Opn]
  , lexExp'' ")(" ~?= return [Cls, Opr Mul, Opn]
  , lexExp'' "-2.3" ~?= return [Num (Z (-1)), Opr Jux, Num (Q 23 10)]
  , lexExp'' "1-2.3" ~?= return [Num (Z 1), Opr Sub, Num (Q 23 10)]
  , lexExp'' "x-2.3" ~?= return [Chr 'x', Opr Sub, Num (Q 23 10)]
  , lexExp'' "+-2.3" ~?= return [Opr Add, Num (Z (-1)), Opr Jux, Num (Q 23 10)]
  , lexExp'' "log-2.3" ~?= return [Fun Log, Opn, Cls, Opr Sub, Num (Q 23 10)]
  , lexExp'' "log()-2.3" ~?= return [Fun Log, Opn, Cls, Opr Sub, Num (Q 23 10)]
  , lexExp'' ",-2.3" ~?= return [Sep, Num (Z (-1)), Opr Jux, Num (Q 23 10)]
  , lexExp'' "(-2.3" ~?= return [Opn, Num (Z (-1)), Opr Jux, Num (Q 23 10)]
  , lexExp'' ")-2.3" ~?= return [Cls, Opr Sub, Num (Q 23 10)]
  , matchNum "" ~?= Nothing
  , matchNum " " ~?= Nothing
  , matchNum "x" ~?= Nothing
  , matchNum "x1" ~?= Nothing
  , matchNum "1" ~?= return (Num (Z 1), "")
  , matchNum "1.2" ~?= return (Num (Q 6 5), "")
  , matchNum "1+" ~?= return (Num (Z 1), "+")
  , matchNum "1.2+" ~?= return (Num (Q 6 5), "+")
  , matchNum "1.2.3" ~?= return (Num (Q 6 5), ".3")
  , matchNum " 1 .2" ~?= return (Num (Z 1), " .2")
  , matchChr "" ~?= Nothing
  , matchChr " " ~?= Nothing
  , matchChr " x" ~?= Nothing
  , matchChr "1" ~?= Nothing
  , matchChr "1x" ~?= Nothing
  , matchChr "x" ~?= return (Chr 'x', "")
  , matchChr "x1" ~?= return (Chr 'x', "1")
  , matchOpr "" ~?= Nothing
  , matchOpr " " ~?= Nothing
  , matchOpr "x" ~?= Nothing
  , matchOpr "x+" ~?= Nothing
  , matchOpr " +" ~?= Nothing
  , matchOpr "+x" ~?= return (Opr Add, "x")
  , matchOpr "++" ~?= return (Opr Add, "+")
  , matchFun "" ~?= Nothing
  , matchFun " " ~?= Nothing
  , matchFun "x" ~?= Nothing
  , matchFun "xlog" ~?= Nothing
  , matchFun " l o g " ~?= Nothing
  , matchFun " log log " ~?= Nothing
  , matchFun "logx" ~?= return (Fun Log, "()x")
  , matchFun "log(x)" ~?= return (Fun Log, "(x)")
  , matchFun "loglog" ~?= return (Fun Log, "()log")
  , matchPattern "" "" ~?= return ""
  , matchPattern "" "x" ~?= Nothing
  , matchPattern "x" "" ~?= return "x"
  , matchPattern "x" "x" ~?= return ""
  , matchPattern "x" "xy" ~?= Nothing
  , matchPattern "xy" "x" ~?= return "y"
  , matchPattern "xy" "xy" ~?= return "" ]