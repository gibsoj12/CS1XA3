{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}



module ExprDiff where
import ExprType
import ExprEval
import qualified Data.Map as Map

{-
Class DiffExpr:
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

class (EvalExpr a) => DiffExprs a where
    partDiff :: String -> Expr a -> Expr a
{- Default Methods -}




{-Most intuitive instance of DiffExpr
-Num instance only relies on +
-Methods:
-   eval : 
-   simplify :
-   partDiff : 
-}



instance DiffExpr Double where --Might want to split into something like integral a and floating a
    eval vrs (Add e1 e2)            = eval vrs e1 + eval vrs e2
    eval vrs (Mult e1 e2)           = eval vrs e1 * eval vrs e2
    eval vrs (Const x)              = x
    eval vrs (Var x)                = case map.lookup x vrs of
                                        Just v  -> v
                                        Nothing -> error "failed lookup in eval"
    eval vrs (Sine e1)              = 
    simplify _ e                    = e -- #TODO finish
    partDiff _ e                    = e -- #TODO finish 

