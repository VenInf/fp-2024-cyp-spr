module Expr.MyAST where

import Text.Printf (printf)

data BinOpeation = Plus
                  | Minus                 
                  | Mult 
                  | Div 
                  | Pow deriving (Show, Eq)

data UnOperation = 
    Sqrt deriving (Show, Eq)

data OpChioce = Bin BinOpeation | Un UnOperation deriving (Show, Eq) -- is there any better way to make a union?

data Expr a = Var String
            | Const a 
            | UnOp UnOperation (Expr a)
            | BinOp BinOpeation (Expr a) (Expr a) deriving (Show, Eq)


printOp :: OpChioce -> String
printOp (Bin Plus) = "+"
printOp (Bin Minus) = "-"
printOp (Bin Mult) = "*"
printOp (Bin Div) = "/"
printOp (Bin Pow) = "^"

printOp (Un Sqrt) = "sqrt"

printInfix :: (Show a) => Expr a -> String
printInfix (BinOp op l r) = printf "(%s %s %s)" (printInfix l) (printOp (Bin op)) (printInfix r)
printInfix (UnOp op ex) = printf "(%s %s)" (printOp (Un op)) (printInfix ex)
printInfix (Const n) = show n
printInfix (Var s) = s


printPrefix :: (Show a) => Expr a -> String
printPrefix (BinOp op l r) = printf "%s %s %s" (printOp (Bin op)) (printPrefix l) (printPrefix r)
printPrefix (UnOp op ex) = printf "%s %s" (printOp (Un op)) (printPrefix ex)
printPrefix (Const n) = show n
printPrefix (Var s) = s
