module ExprParser (parseExprD, parseExprF) where

import ExprType
import Text.Parsec
import Text.Parsec.String


{- Parser
---------------------------
-Takes a string of format:
    (decide on format)
-parses an expression of Expr
-}

parseExprD :: String -> Expr Double
parseExprD ss           = case parse exprD "" ss of
                            Left err -> error $ show err
                            Right expr -> expr

parseExprF :: String -> Expr Float
parseExprF ss           = case parse exprF "" ss of
                            Left err -> error $ show err
                            Right expr -> expr

exprD :: Parser (Expr Double)
exprD = error "define this error"

exprF :: Parser (Expr Float)
exprF = error "Define this error as well"
