{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

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
import GHC.Generics
import Generic.Random.Generic



instance Arbitrary (Expr Double) where
    arbitrary   = genericArbitraryRec uniform `withBaseCase` return (Const 1)

simplifyProp1 :: Expr Double -> Bool
simplifyProp1 e     = simplify vrs e == simplify vrs (simplify vrs e) || simplify vrs e == error "Failed lookup in eval"
    where vrs = Map.fromList []

