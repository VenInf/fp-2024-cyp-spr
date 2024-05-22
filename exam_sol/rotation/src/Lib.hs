module Lib (someFunc, myRotate) where

import Data.List.HT (rotate, takeRev, dropRev)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- myRotate :: Int -> [a] -> [a]
-- myRotate = rotate -- it can be implemented using takeRev and dropRev 


myRotate :: Int -> [a] -> [a]
myRotate _ [] = []
myRotate 0 l = l
myRotate n l
    | n > length l = myRotate (n `mod` length l) l
    | n < (-1) * length l = myRotate (n `mod` length l) l 

    | n < 0 =  takeRev ((-1) * n) l ++ dropRev ((-1) * n) l
    | n > 0 = drop n l ++ take n l
