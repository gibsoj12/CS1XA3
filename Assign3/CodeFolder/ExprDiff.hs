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
-partDiff : given a var identifier, differentiate IN TERMS of that identifier


-}

class (EvalExpr a) => DiffExpr a where
    partDiff :: String -> Expr a -> Expr a
{- Default Methods -}

instance (ForceFit a) => DiffExpr a where
    partDiff vrs (Add e1 e2)            = Add (partDiff vrs e1) (partDiff vrs e2) --Just evaluate the partial differentiation of each expression
    partDiff vrs (Mult e1 e2)           = Add (Mult e2 (partDiff vrs e1)) (Mult e1 (partDiff vrs e2)) -- Treat first expression as constant, then treat second expression as constant
    partDiff vrs (Cos e)                = Mult (partDiff vrs e) (Neg (Sin e)) -- differentiate the inner expression, then multiply by the negative sin of the original expression
    partDiff vrs (Sin e)                = Mult (partDiff vrs e) (Cos e) -- Same as above, however the derivative of sin is cos, no negative needed
    partDiff vrs (Exp e)                = Mult (partDiff vrs e) (Exp e) -- differentiate the expression, then multiply by e^expression
    partDiff vrs (Ln e)                 = Mult (partDiff vrs e) (Inv e) -- differentiate the expression, multiply by the inverse of the original expression
    partDiff vrs (Neg e)                = Neg (partDiff vrs e) -- Differentiate the expression, then negate it
    partDiff vrs (Pow e1 e2)            = partDiff (Exp (Mult e1 (Ln e2))) -- Fix this







