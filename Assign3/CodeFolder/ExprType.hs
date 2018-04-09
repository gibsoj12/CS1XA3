module ExprType where
import Data.List
{-
Expression data type
~~~~~~~~~~~~~~~~~~~~

wraps different Operations in a expression tree
        Ops:
Add - Standard addition
Mult - Standard multiplication
Const - wrapper for regular value
Var - String identifier for variables
-}

data Expr a = Add (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
            | Const a
            | Var String
    deriving Eq

{-
        getVars
        ~~~~~~~~
-retrieves variable identifiers from an Expr

-}

{-
getVars :: (Expr a) -> String
getVars (Add e1 e2)     = getVars e1 `union` getVars e2
getVars (Mult e1 e2)    = getVars e1 `union` getVars e2
getVars (Const _)       = []
getVars (Var s)         = 
-}