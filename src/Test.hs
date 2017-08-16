module Test where

import Test.HUnit ((~?=), Test (TestList), runTestTT)

import Equation
import Lex
import Main
import Parse
import Reduce
import Show
import Simplify

--------------------------------------------------------------------------------

lexExp'' :: Input -> Either String Tokens
lexExp'' input = return input >>= fmap fst . lexExp

convertRPN' :: Input -> Either String TokenQueue
convertRPN' input = return input >>= lexExp'' >>= fmap reverse . convertRPN

convertAST' :: Input -> Either String Expression
convertAST' input = return input >>= lexExp'' >>= convertRPN >>= convertAST []

--------------------------------------------------------------------------------

analyse' :: Input -> Either String Expression
analyse' = convertAST'

evaluate' :: Input -> Either String Expression
evaluate' input = analyse' input >>= evaluate

rewrite' :: Input -> Either String Expression
rewrite' input = fmap rewrite (analyse' input)

--------------------------------------------------------------------------------

tests :: Test
tests = TestList . reverse $
  [simplifyTests, parseTests, lexTests]

--------------------------------------------------------------------------------

simplifyTests :: Test
simplifyTests = TestList
  [ evaluate' "1.2+3.4" ~?= return (Val (1.2 + 3.4))
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
  , reduceAdd (Bin Add 0 0) ~?= return 0
  , reduceAdd (Bin Add 0 2.3) ~?= return 2.3
  , reduceAdd (Bin Add 2.3 0) ~?= return 2.3
  , reduceAdd (Bin Add (-2.3) 2.3) ~?= return 0
  , reduceAdd (Bin Add 2.3 (-2.3)) ~?= return 0
  , reduceAdd (Bin Add 1.2 3.4) ~?= return (Bin Add 1.2 3.4)
  , reduceAdd (Bin Add 0 (Var 'x')) ~?= return (Var 'x')
  , reduceAdd (Bin Add (Var 'x') 0) ~?= return (Var 'x')
  , reduceAdd (Bin Add (App Neg [Var 'x']) (Var 'x')) ~?= return 0
  , reduceAdd (Bin Add (Var 'x') (App Neg [Var 'x'])) ~?= return 0
  , reduceAdd (Bin Add (Var 'x') (Var 'y')) ~?= return (Bin Add (Var 'x') (Var 'y'))
  , reduceSub (Bin Sub 0 0) ~?= return 0
  , reduceSub (Bin Sub 0 2.3) ~?= return (-2.3)
  , reduceSub (Bin Sub 2.3 0) ~?= return 2.3
  , reduceSub (Bin Sub 2.3 2.3) ~?= return 0
  , reduceSub (Bin Sub 1.2 3.4) ~?= return (Bin Add 1.2 (negate 3.4))
  , reduceSub (Bin Sub 0 (Var 'x')) ~?= return (-Var 'x')
  , reduceSub (Bin Sub (Var 'x') 0) ~?= return (Var 'x')
  , reduceSub (Bin Sub (Var 'x') (Var 'x')) ~?= return 0
  , reduceSub (Bin Sub (Var 'x') (Var 'y')) ~?= return (Bin Add (Var 'x') (-Var 'y'))
  , reduceMul (Bin Mul 0 0) ~?= return 0
  , reduceMul (Bin Mul 0 2.3) ~?= return 0
  , reduceMul (Bin Mul 2.3 0) ~?= return 0
  , reduceMul (Bin Mul 1 2.3) ~?= return 2.3
  , reduceMul (Bin Mul 2.3 1) ~?= return 2.3
  , reduceMul (Bin Mul (Val (-1)) 2.3) ~?= return (-2.3)
  , reduceMul (Bin Mul 2.3 (Val (-1))) ~?= return (-2.3)
  , reduceMul (Bin Mul (recip 2.3) 2.3) ~?= return 1
  , reduceMul (Bin Mul 2.3 (recip 2.3)) ~?= return 1
  , reduceMul (Bin Mul (-recip 2.3) 2.3) ~?= return (Val (-1))
  , reduceMul (Bin Mul (recip (-2.3)) 2.3) ~?= return (Val (-1))
  , reduceMul (Bin Mul 2.3 (-recip 2.3)) ~?= return (Val (-1))
  , reduceMul (Bin Mul 2.3 (recip (-2.3))) ~?= return (Val (-1))
  , reduceMul (Bin Mul 1.2 3.4) ~?= return (Bin Mul 1.2 3.4)
  , reduceMul (Bin Mul 0 (Var 'x')) ~?= return 0
  , reduceMul (Bin Mul (Var 'x') 0) ~?= return 0
  , reduceMul (Bin Mul 1 (Var 'x')) ~?= return (Var 'x')
  , reduceMul (Bin Mul (Var 'x') 1) ~?= return (Var 'x')
  , reduceMul (Bin Mul (Val (-1)) (Var 'x')) ~?= return (-Var 'x')
  , reduceMul (Bin Mul (Var 'x') (Val (-1))) ~?= return (-Var 'x')
  , reduceMul (Bin Mul (recip (Var 'x')) (Var 'x')) ~?= return 1
  , reduceMul (Bin Mul (Var 'x') (recip (Var 'x'))) ~?= return 1
  , reduceMul (Bin Mul (-recip (Var 'x')) (Var 'x')) ~?= return (Val (-1))
  , reduceMul (Bin Mul (recip (-Var 'x')) (Var 'x')) ~?= return (Val (-1))
  , reduceMul (Bin Mul (Var 'x') (-recip (Var 'x'))) ~?= return (Val (-1))
  , reduceMul (Bin Mul (Var 'x') (recip (-Var 'x'))) ~?= return (Val (-1))
  , reduceMul (Bin Mul (Var 'x') (Var 'y')) ~?= return (Bin Mul (Var 'x') (Var 'y'))
  , reduceDiv (Bin Div 0 0) ~?= Left "Reduction error: division by zero"
  , reduceDiv (Bin Div 0 2.3) ~?= return 0
  , reduceDiv (Bin Div 2.3 0) ~?= Left "Reduction error: division by zero"
  , reduceDiv (Bin Div 1 2.3) ~?= return (recip 2.3)
  , reduceDiv (Bin Div 2.3 1) ~?= return 2.3
  , reduceDiv (Bin Div 2.3 2.3) ~?= return 1
  , reduceDiv (Bin Div (Val (-1)) 2.3) ~?= return (-recip 2.3)
  , reduceDiv (Bin Div 2.3 (Val (-1))) ~?= return (-2.3)
  , reduceDiv (Bin Div (-2.3) 2.3) ~?= return (Val (-1))
  , reduceDiv (Bin Div 2.3 (-2.3)) ~?= return (Val (-1))
  , reduceDiv (Bin Div 1.2 3.4) ~?= return (Bin Mul 1.2 (recip 3.4))
  , reduceDiv (Bin Div 0 (Var 'x')) ~?= return 0
  , reduceDiv (Bin Div (Var 'x') 0) ~?= Left "Reduction error: division by zero"
  , reduceDiv (Bin Div 1 (Var 'x')) ~?= return (recip (Var 'x'))
  , reduceDiv (Bin Div (Var 'x') 1) ~?= return (Var 'x')
  , reduceDiv (Bin Div (Var 'x') (Var 'x')) ~?= return 1
  , reduceDiv (Bin Div (Val (-1)) (Var 'x')) ~?= return (-recip (Var 'x'))
  , reduceDiv (Bin Div (Var 'x') (Val (-1))) ~?= return (-Var 'x')
  , reduceDiv (Bin Div (-Var 'x') (Var 'x')) ~?= return (Val (-1))
  , reduceDiv (Bin Div (Var 'x') (-Var 'x')) ~?= return (Val (-1))
  , reduceDiv (Bin Div (Var 'x') (Var 'y')) ~?= return (Bin Mul (Var 'x') (recip (Var 'y')))
  , reduceExp (Bin Exp 0 0) ~?= Left "Reduction error: zero power zero"
  , reduceExp (Bin Exp (Val (-2.3)) 0) ~?= Left "Reduction error: negative exponent base"
  , reduceExp (Bin Exp 0 2.3) ~?= return 0
  , reduceExp (Bin Exp 2.3 0) ~?= return 1
  , reduceExp (Bin Exp (Val (-2.3)) 1) ~?= Left "Reduction error: negative exponent base"
  , reduceExp (Bin Exp 1 2.3) ~?= return 1
  , reduceExp (Bin Exp 2.3 1) ~?= return 2.3
  , reduceExp (Bin Exp (Val (-2.3)) (Val (-1))) ~?= Left "Reduction error: negative exponent base"
  , reduceExp (Bin Exp (Val (-1)) 2.3) ~?= Left "Reduction error: negative exponent base"
  , reduceExp (Bin Exp 2.3 (Val (-1))) ~?= return (recip 2.3)
  , reduceExp (Bin Exp (Val (-1.2)) 3.4) ~?= Left "Reduction error: negative exponent base"
  , reduceExp (Bin Exp 1.2 3.4) ~?= return (Bin Exp 1.2 3.4)
  , reduceExp (Bin Exp 0 (Var 'x')) ~?= return 0
  , reduceExp (Bin Exp (Var 'x') 0) ~?= return 1
  , reduceExp (Bin Exp 1 (Var 'x')) ~?= return 1
  , reduceExp (Bin Exp (Var 'x') 1) ~?= return (Var 'x')
  , reduceExp (Bin Exp (Var 'x') (Val (-1))) ~?= return (recip (Var 'x'))
  , reduceExp (Bin Exp (Val (-2.3)) (Var 'x')) ~?= Left "Reduction error: negative exponent base"
  , reduceExp (Bin Exp (Var 'x') (Var 'x')) ~?= return (Bin Exp (Var 'x') (Var 'x'))
  , reduceMul (Bin Jux 0 0) ~?= return 0
  , reduceMul (Bin Jux 0 2.3) ~?= return 0
  , reduceMul (Bin Jux 2.3 0) ~?= return 0
  , reduceMul (Bin Jux 1 2.3) ~?= return 2.3
  , reduceMul (Bin Jux 2.3 1) ~?= return 2.3
  , reduceMul (Bin Jux (Val (-1)) 2.3) ~?= return (-2.3)
  , reduceMul (Bin Jux 2.3 (Val (-1))) ~?= return (-2.3)
  , reduceMul (Bin Jux (recip 2.3) 2.3) ~?= return 1
  , reduceMul (Bin Jux 2.3 (recip 2.3)) ~?= return 1
  , reduceMul (Bin Mul (-recip 2.3) 2.3) ~?= return (Val (-1))
  , reduceMul (Bin Jux (recip (-2.3)) 2.3) ~?= return (Val (-1))
  , reduceMul (Bin Jux 2.3 (-recip 2.3)) ~?= return (Val (-1))
  , reduceMul (Bin Jux 2.3 (recip (-2.3))) ~?= return (Val (-1))
  , reduceMul (Bin Jux 1.2 3.4) ~?= return (Bin Jux 1.2 3.4)
  , reduceMul (Bin Jux 0 (Var 'x')) ~?= return 0
  , reduceMul (Bin Jux (Var 'x') 0) ~?= return 0
  , reduceMul (Bin Jux 1 (Var 'x')) ~?= return (Var 'x')
  , reduceMul (Bin Jux (Var 'x') 1) ~?= return (Var 'x')
  , reduceMul (Bin Jux (Val (-1)) (Var 'x')) ~?= return (-Var 'x')
  , reduceMul (Bin Jux (Var 'x') (Val (-1))) ~?= return (-Var 'x')
  , reduceMul (Bin Jux (recip (Var 'x')) (Var 'x')) ~?= return 1
  , reduceMul (Bin Jux (Var 'x') (recip (Var 'x'))) ~?= return 1
  , reduceMul (Bin Jux (-recip (Var 'x')) (Var 'x')) ~?= return (Val (-1))
  , reduceMul (Bin Jux (recip (-Var 'x')) (Var 'x')) ~?= return (Val (-1))
  , reduceMul (Bin Jux (Var 'x') (-recip (Var 'x'))) ~?= return (Val (-1))
  , reduceMul (Bin Jux (Var 'x') (recip (-Var 'x'))) ~?= return (Val (-1))
  , reduceMul (Bin Jux (Var 'x') (Var 'y')) ~?= return (Bin Jux (Var 'x') (Var 'y'))
  , reduceLog (App Log [0, 0]) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [0, 4]) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [2, 0]) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [0, Var 'x']) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [Var 'x', 0]) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [Val (-1), Val (-1)]) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [0, Val (-1)]) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [Val (-1), 0]) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [2, Val (-1)]) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [Val (-1), 4]) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [Var 'x', Val (-1)]) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [Val (-1), Var 'x']) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [1, 1]) ~?= Left "Reduction error: log base one"
  , reduceLog (App Log [0, 1]) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [1, 0]) ~?= Left "Reduction error: log base one"
  , reduceLog (App Log [Val (-1), 1]) ~?= Left "Reduction error: log of non positive"
  , reduceLog (App Log [1, Val (-1)]) ~?= Left "Reduction error: log base one"
  , reduceLog (App Log [1, 4]) ~?= Left "Reduction error: log base one"
  , reduceLog (App Log [1, Var 'x']) ~?= Left "Reduction error: log base one"
  , reduceLog (App Log [2, 2]) ~?= return 1
  , reduceLog (App Log [2, 1]) ~?= return 0
  , reduceLog (App Log [Var 'x', 1]) ~?= return 0
  , reduceLog (App Log [Var 'x', Var 'x']) ~?= return 1
  , rewrite' "1+x" ~?= return (App Sum [1, Con 'x'])
  , rewrite' "1+x+2" ~?= return (App Sum [1, 2, Con 'x'])
  , rewrite' "1+x+2+y" ~?= return (App Sum [1, 2, Con 'x', Con 'y'])
  , rewrite' "1+x+2+y+3" ~?= return (App Sum [1, 2, 3, Con 'x', Con 'y'])
  , rewrite' "1+((x+(2+y))+3)" ~?= return (App Sum [1, 2, 3, Con 'x', Con 'y'])
  , rewrite' "1*x" ~?= return (App Prd [1, Con 'x'])
  , rewrite' "1*x*2" ~?= return (App Prd [1, 2, Con 'x'])
  , rewrite' "1*x*2*y" ~?= return (App Prd [1, 2, Con 'x', Con 'y'])
  , rewrite' "1*x*2*y*3" ~?= return (App Prd [1, 2, 3, Con 'x', Con 'y'])
  , rewrite' "1*((x*(2*y))*3)" ~?= return (App Prd [1, 2, 3, Con 'x', Con 'y'])
  , rewrite' "1+x*2" ~?= return (App Sum [1, App Prd [2, Con 'x']])
  , rewrite' "1*x+2" ~?= return (App Sum [2, App Prd [1, Con 'x']])
  , rewrite' "1+x+2*y" ~?= return (App Sum [1, Con 'x', App Prd [2, Con 'y']])
  , rewrite' "1+x*2+y" ~?= return (App Sum [1, Con 'y', App Prd [2, Con 'x']])
  , rewrite' "1*x+2+y" ~?= return (App Sum [2, Con 'y', App Prd [1, Con 'x']])
  , rewrite' "1+x*2*y" ~?= return (App Sum [1, App Prd [2, Con 'x', Con 'y']])
  , rewrite' "1*x+2*y" ~?= return (App Sum [App Prd [1, Con 'x'], App Prd [2, Con 'y']])
  , rewrite' "1*x*2+y" ~?= return (App Sum [Con 'y', App Prd [1, 2, Con 'x']])
  , rewrite' "1+x+2+y*3" ~?= return (App Sum [1, 2, Con 'x', App Prd [3, Con 'y']])
  , rewrite' "1+x+2*y+3" ~?= return (App Sum [1, 3, Con 'x', App Prd [2, Con 'y']])
  , rewrite' "1+x*2+y+3" ~?= return (App Sum [1, 3, Con 'y', App Prd [2, Con 'x']])
  , rewrite' "1*x+2+y+3" ~?= return (App Sum [2, 3, Con 'y', App Prd [1, Con 'x']])
  , rewrite' "1+x+2*y*3" ~?= return (App Sum [1, Con 'x', App Prd [2, 3, Con 'y']])
  , rewrite' "1+x*2+y*3" ~?= return (App Sum [1, App Prd [2, Con 'x'], App Prd [3, Con 'y']])
  , rewrite' "1*x+2+y*3" ~?= return (App Sum [2, App Prd [1, Con 'x'], App Prd [3, Con 'y']])
  , rewrite' "1+x*2*y+3" ~?= return (App Sum [1, 3, App Prd [2, Con 'x', Con 'y']])
  , rewrite' "1*x+2*y+3" ~?= return (App Sum [3, App Prd [1, Con 'x'], App Prd [2, Con 'y']])
  , rewrite' "1*x*2+y+3" ~?= return (App Sum [3, Con 'y', App Prd [1, 2, Con 'x']])
  , rewrite' "1+x*2*y*3" ~?= return (App Sum [1, App Prd [2, 3, Con 'x', Con 'y']])
  , rewrite' "1*x+2*y*3" ~?= return (App Sum [App Prd [1, Con 'x'], App Prd [2, 3, Con 'y']])
  , rewrite' "1*x*2+y*3" ~?= return (App Sum [App Prd [1, 2, Con 'x'], App Prd [3, Con 'y']])
  , rewrite' "1*x*2*y+3" ~?= return (App Sum [3, App Prd [1, 2, Con 'x', Con 'y']])
  , rewrite' "1+x-2" ~?= return (Bin Sub (App Sum [1, Con 'x']) 2)
  , rewrite' "1-x+2" ~?= return (App Sum [2, Bin Sub 1 (Con 'x')])
  , rewrite' "1+x+2-y" ~?= return (Bin Sub (App Sum [1, 2, Con 'x']) (Con 'y'))
  , rewrite' "1+x-2+y" ~?= return (App Sum [Con 'y', Bin Sub (App Sum [1, Con 'x']) 2])
  , rewrite' "1-x+2+y" ~?= return (App Sum [2, Con 'y', Bin Sub 1 (Con 'x')])
  , rewrite' "1+x-2-y" ~?= return (Bin Sub (Bin Sub (App Sum [1, Con 'x']) 2) (Con 'y'))
  , rewrite' "1-x+2-y" ~?= return (Bin Sub (App Sum [2, Bin Sub 1 (Con 'x')]) (Con 'y'))
  , rewrite' "1-x-2+y" ~?= return (App Sum [Con 'y', Bin Sub (Bin Sub 1 (Con 'x')) 2])
  , rewrite' "1+x+2+y-3" ~?= return (Bin Sub (App Sum [1, 2, Con 'x', Con 'y']) 3)
  , rewrite' "1+x+2-y+3" ~?= return (App Sum [3, Bin Sub (App Sum [1, 2, Con 'x']) (Con 'y')])
  , rewrite' "1+x-2+y+3" ~?= return (App Sum [3, Con 'y', Bin Sub (App Sum [1, Con 'x']) 2])
  , rewrite' "1-x+2+y+3" ~?= return (App Sum [2, 3, Con 'y', Bin Sub 1 (Con 'x')])
  , rewrite' "1+x+2-y-3" ~?= return (Bin Sub (Bin Sub (App Sum [1, 2, Con 'x']) (Con 'y')) 3)
  , rewrite' "1+x-2+y-3" ~?= return (Bin Sub (App Sum [Con 'y', Bin Sub (App Sum [1, Con 'x']) 2]) 3)
  , rewrite' "1-x+2+y-3" ~?= return (Bin Sub (App Sum [2, Con 'y', Bin Sub 1 (Con 'x')]) 3)
  , rewrite' "1+x-2-y+3" ~?= return (App Sum [3, Bin Sub (Bin Sub (App Sum [1, Con 'x']) 2) (Con 'y')])
  , rewrite' "1-x+2-y+3" ~?= return (App Sum [3, Bin Sub (App Sum [2, Bin Sub 1 (Con 'x')]) (Con 'y')])
  , rewrite' "1-x-2+y+3" ~?= return (App Sum [3, Con 'y', Bin Sub (Bin Sub 1 (Con 'x')) 2])
  , rewrite' "1+x-2-y-3" ~?= return (Bin Sub (Bin Sub (Bin Sub (App Sum [1, Con 'x']) 2) (Con 'y')) 3)
  , rewrite' "1-x+2-y-3" ~?= return (Bin Sub (Bin Sub (App Sum [2, Bin Sub 1 (Con 'x')]) (Con 'y')) 3)
  , rewrite' "1-x-2+y-3" ~?= return (Bin Sub (App Sum [Con 'y', Bin Sub (Bin Sub 1 (Con 'x')) 2]) 3)
  , rewrite' "1-x-2-y+3" ~?= return (App Sum [3, Bin Sub (Bin Sub (Bin Sub 1 (Con 'x')) 2) (Con 'y')])
  , rewrite' "1*x/2" ~?= return (Bin Div (App Prd [1, Con 'x']) 2)
  , rewrite' "1/x*2" ~?= return (App Prd [2, Bin Div 1 (Con 'x')])
  , rewrite' "1*x*2/y" ~?= return (Bin Div (App Prd [1, 2, Con 'x']) (Con 'y'))
  , rewrite' "1*x/2*y" ~?= return (App Prd [Con 'y', Bin Div (App Prd [1, Con 'x']) 2])
  , rewrite' "1/x*2*y" ~?= return (App Prd [2, Con 'y', Bin Div 1 (Con 'x')])
  , rewrite' "1*x/2/y" ~?= return (Bin Div (Bin Div (App Prd [1, Con 'x']) 2) (Con 'y'))
  , rewrite' "1/x*2/y" ~?= return (Bin Div (App Prd [2, Bin Div 1 (Con 'x')]) (Con 'y'))
  , rewrite' "1/x/2*y" ~?= return (App Prd [Con 'y', Bin Div (Bin Div 1 (Con 'x')) 2])
  , rewrite' "1*x*2*y/3" ~?= return (Bin Div (App Prd [1, 2, Con 'x', Con 'y']) 3)
  , rewrite' "1*x*2/y*3" ~?= return (App Prd [3, Bin Div (App Prd [1, 2, Con 'x']) (Con 'y')])
  , rewrite' "1*x/2*y*3" ~?= return (App Prd [3, Con 'y', Bin Div (App Prd [1, Con 'x']) 2])
  , rewrite' "1/x*2*y*3" ~?= return (App Prd [2, 3, Con 'y', Bin Div 1 (Con 'x')])
  , rewrite' "1*x*2/y/3" ~?= return (Bin Div (Bin Div (App Prd [1, 2, Con 'x']) (Con 'y')) 3)
  , rewrite' "1*x/2*y/3" ~?= return (Bin Div (App Prd [Con 'y', Bin Div (App Prd [1, Con 'x']) 2]) 3)
  , rewrite' "1/x*2*y/3" ~?= return (Bin Div (App Prd [2, Con 'y', Bin Div 1 (Con 'x')]) 3)
  , rewrite' "1*x/2/y*3" ~?= return (App Prd [3, Bin Div (Bin Div (App Prd [1, Con 'x']) 2) (Con 'y')])
  , rewrite' "1/x*2/y*3" ~?= return (App Prd [3, Bin Div (App Prd [2, Bin Div 1 (Con 'x')]) (Con 'y')])
  , rewrite' "1/x/2*y*3" ~?= return (App Prd [3, Con 'y', Bin Div (Bin Div 1 (Con 'x')) 2])
  , rewrite' "1*x/2/y/3" ~?= return (Bin Div (Bin Div (Bin Div (App Prd [1, Con 'x']) 2) (Con 'y')) 3)
  , rewrite' "1/x*2/y/3" ~?= return (Bin Div (Bin Div (App Prd [2, Bin Div 1 (Con 'x')]) (Con 'y')) 3)
  , rewrite' "1/x/2*y/3" ~?= return (Bin Div (App Prd [Con 'y', Bin Div (Bin Div 1 (Con 'x')) 2]) 3)
  , rewrite' "1/x/2/y*3" ~?= return (App Prd [3, Bin Div (Bin Div (Bin Div 1 (Con 'x')) 2) (Con 'y')]) ]

--------------------------------------------------------------------------------

parseTests :: Test
parseTests = TestList
  [ parseEqn "" ([], []) ~?= Left "Parse error: miscounted tokens"
  , parseEqn "" ([], [Num (Q 1 2)]) ~?= Left "Parse error: miscounted tokens"
  , parseEqn "" ([Opr Add], [Num (Q 1 2)]) ~?= Left "Parse error: miscounted tokens"
  , parseEqn "" ([Num (Q 1 2)], []) ~?= Left "Parse error: miscounted tokens"
  , parseEqn "" ([Num (Q 1 2)], [Opr Add]) ~?= Left "Parse error: miscounted tokens"
  , parseEqn "" ([Num (Q 1 2)], [Num (Q 1 2)]) ~?= return (Eqn (Val (Q 1 2)) (Val (Q 1 2)))
  , parseExp "" [] ~?= Left "Parse error: miscounted tokens"
  , parseExp "" [Num (Q 1 2)] ~?= return (Val (Q 1 2))
  , parseExp "" [Chr 'x'] ~?= return (Con 'x')
  , parseExp "x" [Chr 'x'] ~?= return (Var 'x')
  , parseExp "" [Opr Add] ~?= Left "Parse error: miscounted tokens"
  , parseExp "" [Opr Add, Chr 'x'] ~?= Left "Parse error: miscounted tokens"
  , parseExp "" [Chr 'x', Opr Add] ~?= Left "Parse error: miscounted tokens"
  , parseExp "" [Chr 'x', Opr Add, Chr 'y'] ~?= return (Bin Add (Con 'x') (Con 'y'))
  , parseExp "" [Fun Log] ~?= Left "Parse error: miscounted tokens"
  , parseExp "" [Fun Log, Chr 'x'] ~?= Left "Parse error: miscounted tokens"
  , parseExp "" [Fun Log, Sep] ~?= Left "Parse error: misplaced separator or missing open bracket"
  , parseExp "" [Fun Log, Opn] ~?= Left "Parse error: missing close bracket"
  , parseExp "" [Fun Log, Cls] ~?= Left "Parse error: missing open bracket"
  , parseExp "" [Fun Log, Opn, Sep] ~?= Left "Parse error: missing close bracket"
  , parseExp "" [Fun Log, Sep, Cls] ~?= Left "Parse error: misplaced separator or missing open bracket"
  , parseExp "" [Fun Log, Opn, Cls] ~?= Left "Parse error: miscounted tokens"
  , parseExp "" [Fun Log, Opn, Sep, Cls] ~?= Left "Parse error: miscounted tokens"
  , parseExp "" [Fun Log, Opn, Chr 'x', Cls] ~?= Left "Parse error: miscounted tokens"
  , parseExp "" [Fun Log, Opn, Chr 'x', Sep, Cls] ~?= Left "Parse error: miscounted tokens"
  , parseExp "" [Fun Log, Opn, Sep, Chr 'x', Cls] ~?= Left "Parse error: miscounted tokens"
  , parseExp "" [Fun Log, Opn, Chr 'x', Chr 'y', Chr 'z', Cls] ~?= Left "Parse error: non empty stack"
  , parseExp "" [Fun Log, Opn, Sep, Chr 'x', Chr 'y', Cls] ~?= return (App Log [Con 'x', Con 'y'])
  , parseExp "" [Fun Log, Opn, Chr 'x', Chr 'y', Sep, Cls] ~?= return (App Log [Con 'x', Con 'y'])
  , parseExp "" [Fun Log, Chr 'x', Chr 'y'] ~?= return (App Log [Con 'x', Con 'y'])
  , parseExp "" [Fun Log, Opn, Chr 'x', Chr 'y', Cls] ~?= return (App Log [Con 'x', Con 'y'])
  , parseExp "" [Fun Log, Opn, Chr 'x', Sep, Chr 'y', Cls] ~?= return (App Log [Con 'x', Con 'y'])
  , convertRPN' "(" ~?= Left "Parse error: missing close bracket"
  , convertRPN' ")" ~?= Left "Parse error: missing open bracket"
  , convertRPN' ")(" ~?= Left "Parse error: missing open bracket"
  , convertRPN' "," ~?= Left "Parse error: misplaced separator or missing open bracket"
  , convertRPN' "(," ~?= Left "Parse error: missing close bracket"
  , convertRPN' ",)" ~?= Left "Parse error: misplaced separator or missing open bracket"
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
  , convertAST' "" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "(" ~?= Left "Parse error: missing close bracket"
  , convertAST' ")" ~?= Left "Parse error: missing open bracket"
  , convertAST' ")(" ~?= Left "Parse error: missing open bracket"
  , convertAST' "()" ~?= Left "Parse error: miscounted tokens"
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
  , convertAST' "+" ~?= Left "Parse error: miscounted tokens"
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
  , convertAST' "log" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "logx" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "xlog" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "log x" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "logxy" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "xlogy" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "xylog" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "log x y" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "log()" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "log(x)" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "log(x)y" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "xlog(y)" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "log(x)(y)" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "log(x,y,z)" ~?= Left "Parse error: non empty stack"
  , convertAST' "log(x)(y)(z)" ~?= Left "Parse error: miscounted tokens"
  , convertAST' "log(x)(y,z)" ~?= Left "Parse error: non empty stack"
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
  , popArguments (-1) "" [] ~?= Left "Parse error: miscounted tokens"
  , popArguments (-1) "" [Num (Z 1)] ~?= Left "Parse error: miscounted tokens"
  , popArguments 0 "" [] ~?= return ([], [])
  , popArguments 0 "" (map (Num . Z) [1 .. 2]) ~?= return ([], [Num (Z 1), Num (Z 2)])
  , popArguments 2 "" [] ~?= Left "Parse error: miscounted tokens"
  , popArguments 2 "" [Num (Z 1)] ~?= Left "Parse error: miscounted tokens"
  , popArguments 2 "" (map (Num . Z) [1 .. 2]) ~?= return ([Val (Z 1), Val (Z 2)], [])
  , popArguments 2 "" (map (Num . Z) [1 .. 4]) ~?= return ([Val (Z 1), Val (Z 2)], [Num (Z 3), Num (Z 4)]) ]

--------------------------------------------------------------------------------

lexTests :: Test
lexTests = TestList
  [ lexEqn " " ~?= Left "Lexical error: error in LHS"
  , lexEqn "=" ~?= Left "Lexical error: error in LHS"
  , lexEqn "x=" ~?= Left "Lexical error: error in RHS"
  , lexEqn "=x" ~?= Left "Lexical error: error in LHS"
  , lexEqn "==" ~?= Left "Lexical error: error in LHS"
  , lexEqn "x==" ~?= Left "Lexical error: error in RHS"
  , lexEqn "=x=" ~?= Left "Lexical error: error in LHS"
  , lexEqn "==x" ~?= Left "Lexical error: error in LHS"
  , lexEqn "x=x=" ~?= Left "Lexical error: error in RHS"
  , lexEqn "x==x" ~?= Left "Lexical error: error in RHS"
  , lexEqn "=x=x" ~?= Left "Lexical error: error in LHS"
  , lexEqn "x=x=x" ~?= Left "Lexical error: error in RHS"
  , lexEqn "===" ~?= Left "Lexical error: error in LHS"
  , lexEqn "x===" ~?= Left "Lexical error: error in RHS"
  , lexEqn "=x==" ~?= Left "Lexical error: error in LHS"
  , lexEqn "==x=" ~?= Left "Lexical error: error in LHS"
  , lexEqn "===x" ~?= Left "Lexical error: error in LHS"
  , lexEqn "x=x==" ~?= Left "Lexical error: error in RHS"
  , lexEqn "x==x=" ~?= Left "Lexical error: error in RHS"
  , lexEqn "x===x" ~?= Left "Lexical error: error in RHS"
  , lexEqn "=x=x=" ~?= Left "Lexical error: error in LHS"
  , lexEqn "=x==x" ~?= Left "Lexical error: error in LHS"
  , lexEqn "==x=x" ~?= Left "Lexical error: error in LHS"
  , lexEqn "x=x=x=" ~?= Left "Lexical error: error in RHS"
  , lexEqn "x=x==x" ~?= Left "Lexical error: error in RHS"
  , lexEqn "x==x=x" ~?= Left "Lexical error: error in RHS"
  , lexEqn "=x=x=x" ~?= Left "Lexical error: error in LHS"
  , lexEqn "x=x=x=x" ~?= Left "Lexical error: error in RHS"
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