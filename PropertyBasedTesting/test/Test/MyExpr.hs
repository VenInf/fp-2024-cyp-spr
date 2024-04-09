module Test.MyExpr where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Expr.MyAST as ST
import qualified Expr.MyParser as Parser

import Debug.Trace

-- import qualified Expr.Prefix as Prefix

-- Here we check that parsers for arythmetics are correct. 
-- It's non-trivial how to generate strings which are likely to be parsed as expressions. 
-- Instead, we generate expressions which are printed into strings, and then parsed. 
-- This is not an ideal solution, because the printer works in one particular way, while 
-- users can do whatever they want. 
-- However this is better than nothing. 

-- For the simplest types, the generator just picks a random value from the list. 
genBinOp :: Gen  ST.BinOpeation
genBinOp = Gen.element [ST.Plus, ST.Minus, ST.Mult, ST.Div, ST.Pow]

genUnOp :: Gen ST.UnOperation
genUnOp = Gen.element [ST.Sqrt]

-- To generate a recursive algebraic data type, use Gen.recursive and Gen.subterm
genExpr :: Int -> Gen (ST.Expr Int)
genExpr n =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      numGen
    , varGen
    ]
    [ -- recursive generators
      binOpGen
    , unOpGen
    ]
  where
    numGen = ST.Const <$> Gen.int (Range.constant 0 n)
    varGen = ST.Var <$> Gen.string (Range.constant 1 n) Gen.alpha

    binOpGen = do
        op <- genBinOp
        Gen.subterm2 (genExpr n) (genExpr n) (ST.BinOp op)
    unOpGen = do
        op <- genUnOp
        Gen.subterm (genExpr n) (ST.UnOp op)

-- parser . printer == id
parserPrinterIsId :: (Show a, Eq a) => MonadTest m => (ST.Expr a -> String) -> (String -> Maybe (String, ST.Expr a)) -> ST.Expr a -> m ()
parserPrinterIsId printer parser ast =
  case parser (printer ast) of
    Just ("", r) -> r === ast
    _ -> failure


prop_printerParserPrefix :: Property
prop_printerParserPrefix = property $ do
  expr <- forAll $ genExpr 100
  parserPrinterIsId ST.printPrefix parser expr
  where
    parser = Parser.runParser Parser.parseExpression

props :: [TestTree]
props = [
    testProperty "`parser . printer == id` for my parser" prop_printerParserPrefix
  ]
