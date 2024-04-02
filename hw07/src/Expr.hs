module Expr where 
import StateDemo ( State, execState, get, modify )
import Data.Maybe ( fromJust )


data BinOpeation = Plus
                  | Minus                 
                  | Mult 
                  | Div 
                  | Pow deriving (Show, Eq)

data UnOperation = 
  Sqrt deriving (Show, Eq)

data Expr a = Var String
            | Const a 
            | UnOp UnOperation (Expr a)
            | Bin BinOpeation (Expr a) (Expr a) deriving (Show, Eq)
        
type ExprState a = [(String, a)]

-- eval :: Expr -> State ExprState Int
-- eval (V v) = do 
--   env <- get 
--   return $ fromJust $ lookup v env
-- eval (C x) = return x 
-- eval (Plus x y) = do 
--   x <- eval x 
--   y <- eval y 
--   return $ x + y 
-- eval (Let x v b) = do 
--   v <- eval v 
--   modify ((x, v) :)
--   eval b 

-- -- runEval (Let "x" (C 13) (Plus (V "x") (C 42)))
-- runEval :: Expr -> Int
-- runEval expr = 
--   execState (eval expr) [] 