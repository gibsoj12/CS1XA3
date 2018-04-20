{-|
Module      : ExprParser
Description : Contains methods for parsing input of the type Expr
Copyright   : (c) Jeff Gibson @2018
License     : WTFPL
Maintainer  : None
Stability   : Experimental
Portability : MSDOS

-}

module ExprParser where

import ExprType
import ExprEval
import Text.Parsec
import Text.Parsec.String


{- Parser
---------------------------
-Takes a string of format:
    >>> \"log(x+1,y)\"
    >>> \"exp(x)\"
-parses an expression of Expr
-}

parseExprDouble :: String -> Expr Double
parseExprDouble ss           = case parse exprD "" ss of
                            Left err -> error $ show err
                            Right expr -> expr


parseExprInteger :: String -> Expr Integer
parseExprInteger ss         = case parse exprInteger "" ss of
                            Left err    -> error $ show err
                            Right expr  -> expr


exprD :: Parser (Expr Double)
exprD         = let
    highest     = parens exprD <|> parseTrigAndLog (parens exprD) <|> parseLogBase exprD <|> parseHighestDouble
    negHighest  = do
        neg <- char '-'
        expr <- highest
        return ((Const (-1)) !* expr)
    powers      = (highest <|> negHighest) `chainl1` powOp
    terms       = powers `chainl1` mulOp
                    in (terms `chainl1` addOp)

exprInteger :: Parser (Expr Integer)
exprInteger         = let
    highest     = parens exprInteger <|> parseTrigAndLog (parens exprInteger) <|> parseLogBase exprInteger <|> parseHighestInt
    negHighest  = do
        neg <- char '-'
        expr <- highest
        return ((Const (-1)) !* expr)
    powers      = (highest <|> negHighest) `chainl1` powOp
    terms       = powers `chainl1` mulOp
                    in (terms `chainl1` addOp)

parseHighestDouble :: Parser (Expr Double)
parseHighestDouble    = (do {cons <- parseConstantDouble; return cons}) <|> (do {v <- many1 alphaNum; return (var v)})

parseHighestInt :: Parser (Expr Integer)
parseHighestInt    = (do {cons <- parseConstantInteger; return cons}) <|> (do {v <- many1 alphaNum; return (var v)})

parseConstantDouble :: Parser (Expr Double)
parseConstantDouble   = do
                cons <- double
                return (val cons)

parseConstantInteger :: Parser (Expr Integer)
parseConstantInteger   = do 
                cons <- integer
                return (val cons)

parseLogBase :: (ForceFit a) => Parser (Expr a) -> Parser (Expr a) -- Used specifically to parse the log with base as this function has a different form from others
parseLogBase parsed             = do {symbol "log(";
                                        first <- parsed;
                                        symbol ",";
                                        second <- parsed;
                                        symbol ")";
                                        return (lawg first second)}

parseTrigAndLog :: (ForceFit a) => Parser (Expr a) -> Parser (Expr a) -- Used to parse unary functions
parseTrigAndLog parsed          =   unaryFuncs "sin" sine parsed 
                                    <|> unaryFuncs "cos" cosine parsed
                                    <|> unaryFuncs "exp" ex parsed
                                    <|> unaryFuncs "inv" Inv parsed
                                    <|> unaryFuncs "ln" ln parsed

unaryFuncs :: String -> (Expr a -> Expr a) -> Parser (Expr a) -> Parser (Expr a) -- Helper to parse unary functions
unaryFuncs str1 func parsed      = do {symbol str1;
                                        apply <- parsed;
                                        return (func apply) }

powOp :: (ForceFit a) => Parser (Expr a -> Expr a -> Expr a) -- Parses power expressions
powOp       = do { symbol "^";
                    return (!^)}

addOp :: (ForceFit a) => Parser (Expr a -> Expr a -> Expr a) -- Parses addition expressions
addOp       = do { symbol "+";
                    return (!+) }
                <|> do { symbol "-"; return (!-) }

mulOp :: (ForceFit a) => Parser (Expr a -> Expr a -> Expr a) -- Parses multiplication expressions
mulOp       = do { symbol "*";
                    return (!*) }
                <|> do { symbol "/"; return (!/) }
 

{-Helper Combinator things-}



parens :: Parser a -> Parser a
parens p = do { symbol "(";
                cs <- p;
                symbol ")";
                return cs }

symbol :: String -> Parser String
symbol ss = let
                symbol' :: Parser String
                symbol' = do { spaces;
                                ss' <- string ss;
                                spaces;
                                return ss' }
            in try symbol'

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                    dig <- digits ;
                    return (neg ++ dig) }
    
integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits

double :: Parser Double
double = fmap read $ do
    natNum <- try negDigits <|> digits
    dot <- (symbol ".") <|> string ""
    dec <- if dot == "." then digits else string ""
    return $ natNum ++ dot ++ dec