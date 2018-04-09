{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}



module ExprDiff where
--import ExprType
import qualified Data.Map as Map

{-Class DiffExpr:
    Differentiable Expressions
----------------------------------
-This class has methods over the Expr datatype
-Assists with construction and evaluation of differentiable expressions

-Methods
-----------------
-eval : takes a dictionary of variable identifiers and values, then uses it to compute the Expr fully
-Simplify : Takes a possibly incomplete dictionary and uses it to reduce Expr as much as possible
    e1 = x + y
    e2 = y + x
    simplify e1 == simplify e2
-partDiff : given a var identifier, differentiate IN TERMS of that identifier
-}

{-

class DiffExprs a where
    eval :: Map.Map String a -> Expr a -> a
    simplify :: Map.Map String a -> Expr a -> Expr a
    partDiff :: String -> Expr a -> Expr a
{- Default Methods -}
    (!+) :: Expr a -> Expr a -> Expr a
    e1 !+ e2        = simplify (Map.fromList []) $ Add e1 e2
    (!*) :: Expr a -> Expr a -> Expr a
    e1 !* e2        = simplify (Map.fromList []) $ Mult e1 e2
    val :: a -> Expr a
    val x           = Const x
    var :: String -> Expr a
    var x           = Var x

-}

{-Most intuitive instance of DiffExpr
-Num instance only relies on +
-Methods:
-   eval : 
-   simplify :
-   partDiff : 
-}

{-

instance (Num a) => DiffExpr a where --Might want to split into something like integral a and floating a
    eval vrs (Add e1 e2)            = eval vrs e1 + eval vrs e2
    eval vrs (Mult e1 e2)           = eval vrs e1 * eval vrs e2
    eval vrs (Const x)              = x
    eval vrs (Var x)                = case map.lookup x vrs of
                                        Just v  -> v
                                        Nothing -> error "failed lookup in eval"
    simplify _ e                    = e -- #TODO finish
    partDiff _ e                    = e -- #TODO finish 

-}