import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit ((@?=), testCase)
import Lib (myRotate)

tests :: TestTree
tests = 
    testGroup "rotate tests" [baseDefTests]
  where
    { 
    baseDefTests = testGroup "Base def tests" [
        testCase "123 -> 123" $ myRotate 0 "123" @?= "123"  
      , testCase "123 -> 231" $ myRotate 1 "123" @?= "231"  
      , testCase "123 -> 312" $ myRotate 2 "123" @?= "312"  
      , testCase "123 -> 312" $ myRotate (-1) "123" @?= "312"
      , testCase "123 -> 231" $ myRotate (-2) "123" @?= "231"  
      , testCase "abcdefghilk -> efghilkabcd" $ myRotate 100500 "abcdefghilk" @?= "efghilkabcd"  
      , testCase "[(1,2), (3,4), (5,6)] -> [(3,4),(5,6),(1,2)]" $ myRotate 1213 [(1,2), (3,4), (5,6)] @?= [(3,4),(5,6),(1,2)] 


    ]
    }

main :: IO ()
main = defaultMain $ testGroup "Tests" [tests]