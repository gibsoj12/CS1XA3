{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}

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
    fmap f (AnError x)        = AnError x
    fmap f (AValue a)         = AValue (f a)

instance Applicative Result where
    pure x                  = AValue x
    (AValue f) <*> x        = fmap f x
    (AnError f) <*> _       = AnError f

{-
Class: EvalExpr
    Simplifiable and evaluatable expressions
-------------------------------------------
-The purpose of this class is to allow for simplification and either partial
-or complete evaluation of expressions
-}

    --Methods
-----------------
--eval : takes a dictionary of variable identifiers and values, then uses it to compute the Expr fully
--Simplify : Takes a possibly incomplete dictionary and uses it to reduce Expr as much as possible
    --e1 = x + y
    --e2 = y + x
    --simplify e1 == simplify e2

class EvalExpr a where
    eval :: Map.Map String a -> Expr a -> Result a
    simplify :: Map.Map String a -> Expr a -> Expr a
    (!+) :: Expr a -> Expr a -> Expr a
    e1 !+ e2        = simplify (Map.fromList []) $ Add e1 e2
    (!-) :: Expr a -> Expr a -> Expr a
    e1 !- e2        = simplify (Map.fromList []) $ Add e1 (Neg e2)
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
    e1 !/ e2        = simplify (Map.fromList []) $ Mult e1 (Inv e2) 
    (!^) :: Expr a -> Expr a -> Expr a
    e1 !^ e2        = simplify (Map.fromList []) $ Pow e1 e2
    ex :: Expr a -> Expr a
    ex e            = Exp e
    val :: a -> Expr a
    val x           = Const x
    var :: String -> Expr a
    var x           = Var x

checkInfinity :: (ForceFit a) => a -> Bool
checkInfinity evaluatedExpr     = let infinity = show evaluatedExpr
                                    in ((infinity == "Infinity") || (infinity == "-Infinity"))

err :: String
err = "Sorry pal, can't do that."

instance (ForceFit a) => EvalExpr a where
    eval vrs (Add e1 e2)        = (+) <$> (eval vrs e1) <*> (eval vrs e2)
    eval vrs (Mult e1 e2)       = (*) <$> (eval vrs e1) <*> (eval vrs e2)
    eval vrs (Neg e)            = (*(-1)) <$> (eval vrs e)
    eval vrs (Sine e)           = fmap forceFitSin (eval vrs e)
    eval vrs (Cosine e)         = fmap forceFitCos (eval vrs e)
    eval vrs (Ln e)             = case (eval vrs e) of
                                        AValue r -> if r <= 0 
                                                        then AnError "Natural log is not defined for values less than or equal to zero"
                                                    else AValue (forceFitLn r)
                                        AnError err -> AnError err
    eval vrs (Lawg e1 e2)       = case (eval vrs e1, eval vrs e2) of
                                        (AValue r1, AValue r2)  -> if (r1 <= 0 || r2 <= 0)
                                                                        then AnError "Logarithm is not defined for values less than or equal to zero"
                                                                   else AValue (forceFitLog r1 r2)
                                        (AnError err, _)        -> AnError err
                                        (_, AnError err)        -> AnError err
    eval vrs (Inv e)            = case (eval vrs e) of 
                                        AValue r    ->  if r == 0
                                                            then AnError "Division by zero error"
                                                        else  AValue (forceFitPow r (-1))
                                        AnError err -> AnError err
    eval vrs (Pow e1 e2)        = case (eval vrs e1, eval vrs e2) of
                                        (AValue r1, AValue r2)  -> if (r1  == 0 && r2 == 0)
                                                                     then AnError "Base zero, exponent zero is undefined."
                                                                   else let value = forceFitPow r1 r2
                                                                        in  if (checkInfinity value)
                                                                                then AnError "Infinity error" 
                                                                            else (AValue value)
                                        (AnError err ,_)        -> AnError err
                                        (_,AnError err)         -> AnError err
    eval vrs (Exp e)            = case (eval vrs e) of
                                        AValue r        -> let value = forceFitExp r
                                                            in  if (checkInfinity value)
                                                                    then AnError "Infinity error."
                                                                else (AValue value)
                                        AnError error   -> AnError err
    eval vrs (Const x)          = AValue x
    eval vrs (Var x)            = case Map.lookup x vrs of
                                        Just v -> AValue v
                                        Nothing -> error "Failed lookup"

    {-Simplification of expressions-}

    --Addition simplification
    
    simplify vrs (Add e1 e2)        = 
        let simpleE1  = simplify vrs e1 -- | Simplify each expression
            simpleE2  = simplify vrs e2
        in case (simpleE1,simpleE2) of
                (Const 0, e)                    -> e -- | x plus 0 is x for every x
                (e, Const 0)                    -> e
                (Const x, Const y)              -> Const (x+y) -- | If both are constants, can combine
                (Const x, Add (Const y) (e))    -> Add (Const (x + y)) (simplify vrs e) -- | ensure that this follows same logic as above
                (Add (Const x) (e), Const y)    -> Add (Const (x + y)) (simplify vrs e) -- | Place this in the above form 
                (Ln x, Ln y)                    -> simplify vrs (Ln (Mult x y)) -- | Place this within a natural log and simplify the inner expression
                (Lawg e1' e2', Lawg e3' e4')    -> if simplify vrs e1' == simplify vrs e2'
                                                        then simplify vrs (Lawg e1' (Mult e2' e4')) -- | If the base is the same then place in this form
                                                    else Add (Lawg e1' e2') (Lawg e3' e4') -- | Otherwise, cannot do any more simplification
                
                (simple1,simple2)               -> Add simple1 simple2 -- | If the expressions do not match above, simply add them
    
    --Negation simplification

    simplify vrs (Neg e)                    = let simpleE = simplify vrs e
                                                in case simpleE of
                                                    (Neg e1)    -> e1 -- | Double negation
                                                    (Const x)   -> Const (-x) -- | Can just be placed as a negative inside of a constant
                                                    (e)         -> Neg e -- | If doesn't match above, leave it

 
    
    --Multiplication simplification
    
    simplify vrs (Mult e1 e2)   = 
        let simpleE1 = simplify vrs e1
            simpleE2 = simplify vrs e2
        in case (simpleE1, simpleE2) of
            (Const x, Const y)  -> Const (x * y) -- | If two constants, return their product
            (Const x, Mult (Const y) e)     -> Mult (Const (x*y)) (simplify vrs e) -- | take product of two constants
            (Mult (Const x) e, Const y)   -> Mult (Const (x*y)) (simplify vrs e) -- | Place in above form
            (Const x, e)    -> if x == 0
                                    then Const 0 -- | If the constant is zero then return zero
                                else if x == 1
                                        then e -- | if the constant is one, return just the other expression
                                else Mult (Const x) e -- | Otherwise, this is the simplest form
            (e, Const x)    -> simplify vrs (Mult (Const x) e) -- | Place into above form 
            (e, Add ex1 ex2)              -> simplify vrs (Add ((Mult e ex1)) (Mult e ex2)) -- | Distribute
            (Add ex1 ex2, e)              -> simplify vrs (Add ((Mult e ex1)) (Mult e ex2)) -- | Place in above form  
            (Pow ex1 ex2, Inv (Pow ex3 ex4)) -> case simplify vrs ex1 == simplify vrs ex3 of
                                                    True        -> simplify vrs (Pow ex1 (Add (ex2) (Neg ex4))) -- | If they have the same base, then we can write as one power expression where the powers are subtracted
                                                    False       -> Mult (Pow (simplify vrs ex1) (simplify vrs ex2)) (Pow (simplify vrs ex3) (simplify vrs ex4))
            (Inv ex1, ex2)                  -> case simplify vrs ex1 == simplify vrs ex2 of
                                                    True        -> Const 1 -- | If the expressions are the same they can be simplified to a constant
                                                    False       -> Mult (simplify vrs (Inv ex1)) (simplify vrs ex2) -- | Otherwise, just simplify the inner expressions and leave them as the product
            (ex1, Inv ex2)                  -> simplify vrs (Mult (Inv ex2) ex1) -- | Place in above form
            (Pow ex1 ex2, Pow ex3 ex4)      -> if simplify vrs ex1 == simplify vrs ex3
                                                    then simplify vrs (Pow ex1 (Add ex2 ex4)) -- | Can put the expressions with the same base, and add their exponents
                                                else Mult (simplify vrs (Pow ex1 ex2)) (simplify vrs (Pow ex3 ex4)) -- | Just simplify the power expressions
            (e1,e2)                         -> Mult e1 e2
    

    --Natural exponent simplification
    simplify vrs (Exp e1)   = 
        let simple1 = simplify vrs e1
        in case simple1 of
            Const 0                     -> Const 1 -- | Exponent of 0 always gives 1
            (Add (Const x) (Const y))   -> simplify vrs (Exp (Const (x+y))) -- | If the exponent is the addition of constants, simplify them and simplify again
            Const x                     -> case eval vrs (Exp (Const x)) of
                                                AValue r    -> (Const r) -- | If you can evaluate, do it
                                                AnError err -> Exp (Const x) -- | Don't want to throw errors in simplify
            Ln e                        -> simplify vrs e -- | exp(Ln) simplifies to the expression inside of the ln
            e                           -> Exp e

    --Power simplification
    simplify vrs (Pow e1 e2)    =
        let simple1 = simplify vrs e1
            simple2 = simplify vrs e2
        in case (simple1,simple2) of
            (_,Const 0)                             -> Const 1 -- | x^0 is 1 for every x
            (Const 0,_)                             -> Const 0 -- | 0 to any exponent is 0
            (Const x, Const y)                      -> case (eval vrs (Pow (Const x) (Const y))) of
                                                            AValue r    -> Const r -- | This is just a constant
                                                            AnError err -> Pow (Const x) (Const y)
            ( e, (Lawg e2 e3))                      -> case (simplify vrs e) == (simplify vrs e1) of
                                                            True    -> simplify vrs e2 -- if bases match, just simplify the expression of the logarithm
                                                            False   -> Pow e1 (Lawg e2 e3)
            (e1,e2)                                 -> Pow e1 e2         

    --Natural Log simplification
    simplify vrs (Ln e1)    = 
        let
            simple1 = simplify vrs e1
        in case simple1 of
            (Exp e)         -> simplify vrs e -- | Simplify just the expression in the exponent
            (Inv e)         -> Neg (simplify vrs (Ln e)) -- | Rewrite this expression
            (Const x)       -> case eval vrs (Ln (Const x)) of
                                    AValue val  -> (Const val) -- If the expression can be evaluated, do so
                                    AnError err -> Ln (Const x)
            e               -> Ln e -- | If it cannot be simplified by above, then don't.


    --Log with Base simplification
    
    simplify vrs (Lawg e1 e2)   =
        let simple1 = simplify vrs e1
            simple2 = simplify vrs e2
        in case (simple1,simple2) of
            (e',Pow e1' e2')        -> case simplify vrs e' == simplify vrs e1' of
                                            True    -> simplify vrs e2' -- If the bases match, can be rewritten
                                            False   -> Lawg e' (Pow e1' e2') -- Bases don't match then just leave them
            (e,Const x)             -> case x <= 0 of
                                            True    -> Lawg e (Const x) -- | If the expression would be an error, just leave it
                                            False   -> case eval vrs (Lawg e (Const x)) of
                                                            AValue r    -> Const r -- | This can be a constant
                                                            AnError err -> Lawg e (Const x)
            (e, (Inv e1'))          -> Neg $ Lawg e e1' -- | Just bring the negative out
            (e1',e2')               -> Lawg e1' e2'

    --Trig functions
    
    simplify vrs (Cosine e) =
        let simple1 = simplify vrs e
        in case simple1 of
            (Const x)           -> case eval vrs (Cosine (Const x)) of
                                        AValue r        -> (Const r) --If given the cos of a constant, return a constant
                                        AnError err     -> Cosine (Const x) -- | Can't happen, still needs a case
            (e)                 -> Cosine e -- | Only simplification case is above
    
    simplify vrs (Sine e) =
        let simple1 = simplify vrs e
        in case simple1 of
            (Const x)           -> case eval vrs (Sine (Const x)) of
                                        AValue r        -> (Const r) -- Same as above for sin
                                        AnError err     -> Sine (Const x) 
            (e)                 -> Sine e -- | Only simplification is above

    
    --Constants simplification

    simplify vrs (Const x)                          = Const x --A constant is the most simplified form

    --Variables simplification

    simplify vrs (Var x)                            = case Map.lookup x vrs of
                                                            Just v -> Const v
                                                            Nothing -> Var x
    
--This idea is taken from Chenc, basically the idea is to force the above instance to work for all nums

class (Num a,Show a,Eq a, Ord a) => ForceFit a where
    forceFitCos :: a -> a -- Cosine
    forceFitSin :: a -> a -- Sine
    forceFitExp :: a -> a -- Natural exponent
    forceFitLn :: a -> a -- Natural logarithm
    forceFitPow :: a -> a -> a -- Power with base 
    forceFitLog :: a -> a -> a -- Logarithm with base

instance ForceFit Double where
    forceFitCos x           = cos x -- For double, everything is really just normal
    forceFitSin x           = sin x -- Same as above
    forceFitExp x           = exp x -- Natural exponent
    forceFitLn x            = log x -- natural log
    forceFitPow x y         = x ** y -- power expression
    forceFitLog x y         = logBase x y -- log with base
 

instance ForceFit Integer where --This instance is different, everything needs to be taken from integral and then rounded
    forceFitCos x           = round $ cos $ fromIntegral x 
    forceFitSin x           = round $ sin $ fromIntegral x 
    forceFitExp x           = round $ exp $ fromIntegral x 
    forceFitLn x            = round $ log $ fromIntegral x 
    forceFitPow x y         = round $ (fromIntegral x) ** (fromIntegral y)
    forceFitLog x y         = round $ logBase (fromIntegral x) (fromIntegral y)