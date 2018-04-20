{-|
Module      : ExprTest
Description : Test cases for parsing, partial diff, simplification and evaluation of an Expr type
Copyright   : (c) Jeff Gibson @2018
License     : WTFPL
Maintainer  : None
Stability   : Experimental
Portability : MSDOS

-}

module ExprTest where

import ExprType
import ExprDiff
import ExprEval
import ExprPretty
import ExprParser
import qualified Data.Map as Map
import Test.QuickCheck

{-
sampleExpr :: Expr Double
sampleExpr      = (var "x") !+ (var "y")


exprProp :: Expr Double -> Bool --Would have to create an instance of arbitrary for quickcheck to generate Expr values

--Write more of these which showcase some functionality, and which you can verify are working correctly


listtoExpr1 :: [Double] -> Expr Double --This approach is better for quickCheck... well easier, not necessarily better
listToExpr1 xs      = 

-}