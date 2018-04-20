{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : ExprType
Description : Contains an expression data type
Copyright   : (c) Jeff Gibson @2018
License     : WTFPL
Maintainer  : None
Stability   : Experimental
Portability : MSDOS

-}

module ExprType where
import Data.List
import GHC.Generics
import Generic.Random.Generic

{-
Expression data type
~~~~~~~~~~~~~~~~~~~~

wraps different Operations in an expression tree
        Ops:
Add - Standard addition
Neg - Standard negation (i.e *-1)
Mult - Standard multiplication
Const - wrapper for regular value
Var - String identifier for variables
Sine - Sin function
Cosine - Cos function
Ln - Natural Logarithmic function
Lawg - Logarithm with base
Inv - Inverse of an expression
Pow - Expression to the exponent of another expression
Exp - e to the exponent of an expression
-}

-- | This module defines the type Expr

data Expr a = Add (Expr a) (Expr a)
            | Neg (Expr a)
            | Mult (Expr a) (Expr a)
            | Const a
            | Var String
            | Sine (Expr a)
            | Cosine (Expr a)
            | Ln (Expr a)
            | Lawg (Expr a) (Expr a) 
            | Inv (Expr a)
            | Pow (Expr a) (Expr a)
            | Exp (Expr a)
    deriving (Eq, Generic)

{-
        getVars
        ~~~~~~~~
-retrieves variable identifiers from an Expr

-}


getVars :: (Expr a) -> [String]
getVars (Add e1 e2)     = getVars e1 `union` getVars e2
getVars (Mult e1 e2)    = getVars e1 `union` getVars e2
getVars (Sine e)        = getVars e
getVars (Cosine e)      = getVars e
getVars (Ln e)          = getVars e
getVars (Lawg e1 e2)    = getVars e1 `union` getVars e2
getVars (Inv e)         = getVars e
getVars (Neg e)         = getVars e
getVars (Pow e1 e2)     = getVars e1 `union` getVars e2
getVars (Exp e)         = getVars e
getVars (Const _)       = []
getVars (Var ident)     = [ident]
