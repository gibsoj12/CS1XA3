{-/
Module      : ExprEval
Description : Contains a type class and instances for evaluation and simplification
Copyright   : (c) Jeff Gibson @2018
License     : WTFPL
Maintainer  : None
Stability   : Experimental
Portability : MSDOS

-}

module ExprEval where
import ExprType
import qualified Data.Map as Map

data Result a = AnError String | AValue a

instance Functor Result where
    fmap f AnError x        = AnError x
    fmap f AValue a         = AValue (f a)

instance Applicative Result where
    pure x                  = AValue x
    (AValue f) <*> x        = fmap f x
    (AnError f) <*> _       = AnError f

class EvalExpr a where
    eval :: Map.Map String a -> Expr a -> Result a
    simplify :: Map.Map String a -> Expr a -> Expr a
    (!+) :: Expr a -> Expr a -> Expr a
    e1 !+ e2        = simplify (Map.fromList []) $ Add e1 e2
    neg :: Expr a -> Expr a
    neg e           = Neg e
    (!*) :: Expr a -> Expr a -> Expr a
    e1 !* e2        = simplify (Map.fromList []) $ Mult e1 e2
    sine :: Expr a -> Expr a
    sine e          = Sine e
    cosine :: Expr a -> Expr a
    cosine e        = Cosine e
    ln :: Expr a -> Expr a
    ln e            = Ln e
    lawg :: Expr a -> Expr a -> Expr a
    lawg e1 e2      = simplify (Map.fromList []) $ Lawg e1 e2
    (!/) :: Expr a -> Expr a -> Expr a
    e1 !/ e2        = simplify (Map.fromList []) $ Mult (e1 (Inv e2) )
    (!^) :: Expr a -> Expr a -> Expr a
    e1 !^ e2        = simplify (Map.fromList []) $ Pow e1 e2
    ex :: Expr a -> Expr a
    ex e            = Exp e
    val :: a -> Expr a
    val x           = Const x
    var :: String -> Expr a
    var x           = Var x

checkInfinity :: (Num a) -> Bool
checkInfinity evaluatedExpr     = let infinity = show evaluatedExpr
                                    in ((infinity == "Infinity") || (infinity == "-Infinity"))

instance EvalExpr Double where
    eval vrs (Add e1 e2)        = fmap (+) ((eval vrs e1) <*> (eval vrs e2))
    eval vrs (Mult e1 e2)       = fmap (*) ((eval vrs e1) <*> (eval vrs e2))
    eval vrs (Neg e)            = fmap (-1*) (eval vrs e)
    eval vrs (Sine e)           = fmap sin (eval vrs e)
    eval vrs (Cosine e)         = fmap cos (eval vrs e)
    eval vrs (Ln e)             = case (eval vrs e) of
                                        AValue r -> if r <= 0 
                                                        then AnError "Natural log is not defined for values less than or equal to zero"
                                                    else AValue (log r)
                                        AnError err -> AnError err
    eval vrs (Lawg e1 e2)       = case (eval vrs e1, eval vrs e2) of
                                        (AValue r1, AValue r2)  -> if (r1 || r2) <= 0
                                                                        then AnError "Logarithm is not defined for values less than or equal to zero"
                                                                   else AValue (logBase r1 r2)
                                        (AnError err, _)        -> AnError err
                                        (_, AnError err)        -> AnError err
    eval vrs (Inv e)            = case (eval vrs e) of 
                                        AValue r    ->  if r == 0
                                                            then AnError "Division by zero error"
                                                        else  AValue (1/r)
                                        AnError err -> AnError err
    eval vrs (Pow e1 e2)        = case (eval vrs e1, eval vrs e2) of
                                        (AValue r1, AValue r2)  -> if (r1 && r2) == 0
                                                                     then AnError "Base zero, exponent zero is undefined."
                                                                   else let value = r1 ** r2
                                                                        in  if (checkInfinity value)
                                                                                then AnError "Infinity error" 
                                                                            else (AValue value)
                                        (AnError err ,_)        -> AnError err
                                        (_,AnError err)         -> AnError err
    eval vrs (Exp e)            = case (eval vrs e) of
                                        AValue r        -> let value = exp r
                                                            in  if (checkInfinity value)
                                                                    then AnError "Infinity error."
                                                                else (AValue value)
                                        AnError error   -> AnError err
    eval vrs (Const x)          = AValue x
    eval vrs (Var x)            = case lookup x vars of
                                        Just v -> AValue v
                                        Nothing -> error "Failed lookup"

    {-Simplification of expressions-}

    --Addition simplification

    simplify vrs (Add (Const 0) e)          = simplify vrs e --x plus 0 is x for every x
    simplify vrs (Add e (Const 0))          = simplify vrs e -- Same as above
    simplify vrs (Add e (Var x))            = Add (simplify vrs e) (Var x) --Simplify the first expression and add variable
    simplify vrs (Add (Var x) e)            = Add (Var x) (simplify vrs e) -- Same as above
    simplify vrs (Add (Const x) (Const y))  = case (eval vrs Add ((Const x) (Const y))) of 
                                                    AValue v    -> Const v --Eval returns a value, return the const value
                                                    AnError err -> error err -- Eval returns an error, present this error
    simplify vrs (Add e1 e2)                = Add (simplify vrs e1) (simplify vrs e2) --Add the two simplified expressions
    simplify vrs (Add (Ln e1) (Ln e2))      = simplify vrs (Ln (Mult e1 e2)) -- Two logs added becomes log of the product of their expressions
    simplify vrs (Add (Lawg e1 e2) (Lawg e3 e4))    = case eval vrs e1 == eval vrs e3 of
                                                            True    -> simplify vrs (Lawg e1 (Mult e2 e3)) --If bases same, can simplify
                                                            False   -> Add (simplify vrs (Lawg e1 e2) (simplify vrs (Lawg e3 e4))) -- Bases different, cannot simplify
    
    --Negation simplification

    simplify vrs (Neg e)                    = Neg (simplify vrs e) --Negate the simplified expression
    
    --Multiplication simplification

    simplify vrs (Mult (Const 0) e)         = case checkInfinity $ eval vrs e of
                                                    True    -> error "0 times infinity error."--Returns an error in case of 0*infinity
                                                    False   -> Const 0 --X multiplied by 0 is 0 for every x (except infinity)
    simplify vrs (Mult e (Const 0))         = case checkInfinity $ eval vrs e of
                                                    True    -> error "0 times infinity error."--Returns an error in case of 0*infinity
                                                    False   -> Const 0 --X multiplied by 0 is 0 for every x (except infinity)
    simplify vrs (Mult (Const 1) e)         = simplify vrs e -- Anything multiplied by 1 is that thing
    simplify vrs (Mult e (Const 1))         = simplify vrs e -- Same as above
    simplify vrs (Mult (Const x) (Const y)) = case eval vrs (Mult (Const x) (Const y)) of
                                                    AValue v    -> Const v --Two constants when multiplied becomes one const
                                                    AnError err -> error err --Case of an error, present this error
    simplify vrs (Mult e (Var x))           = Mult (simplify vars Var x) (simplify vars e) --If given variable, just sub in and multiply by the simplified expression
    simplify vrs (Mult (Var x) e)           = Mult (simplify vars Var x) (simplify vars e) --Same as above
    simplify vrs (Mult (Pow e1 e2) (Pow e3 e4)) = case eval vrs e1 == eval vrs e3 of
                                                    True    -> Pow e1 (simplify vrs (Add e2 e4)) -- If they have the same base, add the exponents (doesn't work if the bases evaluate to the same thing even if they are not the same in variable form)
                                                    False   -> Mult (simplify vrs (Pow e1 e2)) (simplify vrs (Pow e3 e4)) --If the bases are not the same, just simplify the power expressions and multiply them together
    simplify vrs (Mult (Inv Var x) (Var x)) = Const 1 -- X/X is 1
    simplify vrs (Mult (Pow e1 e2) (Inv (Pow e3 e4)))   = case eval vrs e1 == eval vrs e3 of
                                                            True    -> Pow e1 (simplify vrs (Add e1 (Neg e2))) --Same base, subtract exponents
                                                            False   -> Mult (simplify vrs (Pow e1 e2) (simplify vrs (Inv (Pow e3 e4)))) --Otherwise just simplify the power expressions
    simplify vrs (Mult e1 e2)               = Mult (simplify vrs e1) (simplify vrs e2)

    --Natural exponent simplification

    simplify vrs (Exp (Const 0))                    = Const 1 --exponent of 0 always gives 1
    simplify vrs (Exp (Add (Const x) (Const y)))    = case eval vrs (Add (Const x) (Const y)) of
                                                            AValue r    -> simplify vrs (Exp r) -- Take the exponent, evaluate it, then return
                                                            AnError err -> error err 
    simplify vrs (Exp (Var x))                      = Exp (Var x)
    simplify vrs (Exp (Const x))                    = case eval vrs (Exp (Const x)) of
                                                            AValue r    -> Const r -- If you can evaluate, do it
                                                            AnError err -> error err 
    Simplify vrs (Exp (Ln e))                       = case eval vrs e of
                                                            AValue r    -> Const r -- Exponent of Natural Log, just return the evaluation of the expression within the logarithm
                                                            AnError err -> error err 
    simplify vrs (Exp e)                            = Exp (simplify vrs e)

    --Power simplification

    simplify vrs (Pow _ (Const 0))                  = Const 1 --x^0 is 1 for every x
    simplify vrs (Pow (Const 0) _)                  = Const 0 -- 0 to every exponent is 0
    simplify vrs (Pow (Var x) e)                    = Pow (Var x) (simplify vrs e) -- variable to eponent of simplified expression
    simplify vrs (Pow e (Var x))                    = Pow (simplify e) (Var x) -- simplify the expression e and raise it to the exponent x
    simplify vrs (Pow e (Lawg e1 e2))               = case (eval vrs e) == (eval vrs e1) of
                                                            True    -> simplify vrs e2 -- if bases match, just simplify the expression of the logarithm
                                                            False   -> Pow (simplify vrs e) (simplify vrs (Lawg e1 e2))
    simplify vrs (Pow (Const x) (Const y))          = case (eval vrs (Pow (Const x) (Const y))) of 
                                                            AValue r    -> Const r -- If you have a constant with a constant exponent, this can be simplified to a constant
                                                            AnError err -> error err
    simplify vrs (Pow e1 e2)                        = Pow (simplify vrs e1) (simplify vrs e2)

    --Trig simplification

    simplify vrs (Cosine (Var x))                   = Cosine (Var x) -- Just leave this as is, it is a base case
    simplify vrs (Sine (Var x))                     = Sine (Var x) -- Same as above, this is a base case
    simplify vrs (Cosine (Const x))                 = case eval vrs (Cosine (Const x)) of
                                                            AValue r        -> Const r --If given the cos of a constant, return a constant
                                                            AnError err     -> error err
    simplify vrs (Sine (Const x))                   = case eval vrs (Sine (Const x)) of
                                                            AValue r        -> Const r -- Same as above for sin
                                                            AnError err     -> error err
    simplify vrs (Sine e)                           = Sine (simplify vrs e) -- Simplify the inner expression, then Take the sin                          
    simplify vrs (Cosine e)                         = Cosine (simplify vrs e) -- Same as above
    
    --Constants simplification

    simplify vrs (Const x)                          = Const x --A constant is the most simplified form

    --Variables simplification

    simplify vrs (Var x)                            = case lookup x vars of
                                                            Just v -> AValue v
                                                            Nothing -> error "Failed lookup"
    
