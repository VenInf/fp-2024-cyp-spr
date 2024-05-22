module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)
instance Foldable (Tree) where
    foldMap f Leaf = mempty
    foldMap f (Node t1 x t2) = foldMap f t1 <> f x <> foldMap f t1 
  
    foldr f z Leaf = z
    foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l

    