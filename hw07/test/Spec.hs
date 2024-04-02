import Test.Tasty (defaultMain, testGroup, TestTree)
import Parser (runParser, parseExpression)
import Test.Tasty.HUnit ((@?=), testCase)
import Expr (Expr(..), BinOpeation(..), UnOperation(..))


testParser :: TestTree
testParser = 
    testGroup "ParserTests" [baseDefTests]
  where
    { 
    baseDefTests = testGroup "Base def tests" [
        testCase "1 == Const 1" $ runParser parseExpression "1" @?= Just ("", Const 1)
      , testCase "-1 == Const (-1)" $ runParser parseExpression "-1" @?= Just ("", Const (-1))
      , testCase "xyz == Var xyz" $ runParser parseExpression "xyz" @?= Just ("", Var "xyz")
      , testCase "sqrt 101 == UnOp Sqrt 101" $ runParser parseExpression "sqrt 101" @?= Just ("", UnOp Sqrt (Const 101))
      , testCase "sqrt xyz == UnOp Sqrt Var xyz" $ runParser parseExpression "sqrt xyz" @?= Just ("", UnOp Sqrt (Var "xyz"))
      , testCase "+ 1 2 == Bin (+) 1 2" $ runParser parseExpression "+ 1 2" @?= Just ("",  Bin Plus (Const 1) (Const 2))
    
      , testCase "+ 101 45 == Bin (+) 101 45" $ runParser parseExpression "+ 101 45" @?= Just ("", Bin Plus (Const 101) (Const 45))
      , testCase "* xyz 101 == Bin (*) xyz 101" $ runParser parseExpression "* xyz 101" @?= Just ("", Bin Mult (Var "xyz") (Const 101))
      , testCase "/ xyz sqr == Bin (/) xyz sqr" $ runParser parseExpression "/ xyz sqr" @?= Just ("", Bin Div (Var "xyz") (Var "sqr"))
      , testCase "+ 101 * 45 6 == Bin (+) 101 (Bin (*) 45 6)" $ runParser parseExpression "+ 101 * 45 6" @?= Just ("", Bin Plus (Const 101) (Bin Mult (Const 45) (Const 6)))
      , testCase "+ * 101 45 6 == Bin (+) (Bin (*) 101 45) 6" $ runParser parseExpression "+ * 101 45 6" @?= Just ("", Bin Plus (Bin Mult (Const 101) (Const 45)) (Const 6))
      , testCase "/ sqrt 101 xyz == Bin (/) (Sqrt 101) xyz" $ runParser parseExpression "/ sqrt 101 xyz" @?= Just ("", Bin Div (UnOp Sqrt (Const 101)) (Var "xyz"))
      , testCase "/ sqrt -1234 xyz == Bin (/) (Sqrt -1234) xyz" $ runParser parseExpression "/ sqrt -1234 xyz" @?= Just ("", Bin Div (UnOp Sqrt (Const (-1234))) (Var "xyz"))
      
    ]
    }

main :: IO ()
main = defaultMain $ testGroup "Tests" [testParser]