{-|
Module      : ExprPretty
Description : Contains an instance of Show for the Expr type
Copyright   : (c) Jeff Gibson @2018
License     : WTFPL
Maintainer  : None
Stability   : Experimental
Portability : MSDOS

-}

module ExprPretty where

import ExprType

parens :: String -> String
parens s        = "(" ++ s ++ ")"

{-Instance Show Expr
-Provides a nice way of displaying our data type
-Matching the DSL provided in DiffExpr
-}


instance Show a => Show (Expr a) where
    show (Mult (Inv e) e2)  = show e2 ++ " / " ++ show e
    show (Mult e (Inv e1))  = show e ++ " / " ++ show e1
    show (Mult e1 e2)       = parens (show e1) ++ "*" ++ parens (show e2)
    show (Add e1 e2)        = parens (show e1) ++ "+" ++ parens (show e2)
    show (Const x)          = show x
    show (Var s)            = s
    show (Inv e)            = "1/" ++ show e
    show (Neg e)            = " -" ++ show e
    show (Cosine e)         = "cos" ++ parens (show e)
    show (Sine e)           = "sin" ++ parens (show e)
    show (Exp e)            = "exp" ++ parens (show e)
    show (Ln e)             = "ln" ++ parens (show e)
    show (Lawg e1 e2)       = "logBase" ++ parens (show e1) ++ " " ++ parens (show e2)
    show (Pow e1 e2)        = parens (show e1) ++ "^" ++ parens (show e2)


