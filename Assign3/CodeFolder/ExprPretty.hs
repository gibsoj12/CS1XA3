module ExprPretty where

import ExprType

parens :: String -> String
parens s        = "(" ++ s ++ ")"

{-Instance Show Expr
-Provides a nice way of displaying our data type
-Matching the DSL provided in DiffExpr
-}

instance Show a => Show (Expr a) where
    show (Mult e1 e2)   = parens (show e1) ++ " !* " ++ parens (show e2)
    show (Add e1 e2)    = parens (show e1) ++ " !+ " ++ parens (show e2)
    show (Const x)      = parens $ "val " ++ show x
    show (Var s)        = parens $ "var \"" ++ s ++ "\""