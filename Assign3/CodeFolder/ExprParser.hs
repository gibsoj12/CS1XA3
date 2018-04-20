module ExprParser where

import ExprType
import ExprEval
import Text.Parsec
import Text.Parsec.String


{- Parser
---------------------------
-Takes a string of format:
    (decide on format)
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
parseHighestDouble    = (do {cons <- parseConstantDouble; return cons}) <|> (do {var <- many1 alphaNum; return (Var var)})

parseHighestInt :: Parser (Expr Integer)
parseHighestInt    = (do {cons <- parseConstantInteger; return cons}) <|> (do {var <- many1 alphaNum; return (Var var)})

parseConstantDouble :: Parser (Expr Double)
parseConstantDouble   = do
                cons <- double
                return (Const cons)

parseConstantInteger :: Parser (Expr Integer)
parseConstantInteger   = do 
                cons <- integer
                return (Const cons)

parseLogBase :: (ForceFit a) => Parser (Expr a) -> Parser (Expr a)
parseLogBase parsed             = do {symbol "log(";
                                        first <- parsed;
                                        symbol ",";
                                        second <- parsed;
                                        symbol ")";
                                        return (Lawg first second)}

parseTrigAndLog :: (ForceFit a) => Parser (Expr a) -> Parser (Expr a)
parseTrigAndLog parsed          =   unaryFuncs "sin" sine parsed
                                    <|> unaryFuncs "cos" cosine parsed
                                    <|> unaryFuncs "exp" ex parsed
                                    <|> unaryFuncs "inv" Inv parsed
                                    <|> unaryFuncs "ln" ln parsed

unaryFuncs :: String -> (Expr a -> Expr a) -> Parser (Expr a) -> Parser (Expr a)
unaryFuncs str1 func parsed      = do {symbol str1;
                                        apply <- parsed;
                                        return (func apply) }

powOp :: (ForceFit a) => Parser (Expr a -> Expr a -> Expr a)
powOp       = do { symbol "^";
                    return (!^)}

addOp :: (ForceFit a) => Parser (Expr a -> Expr a -> Expr a)
addOp       = do { symbol "+";
                    return (!+) }
                <|> do { symbol "-"; return (!-) }

mulOp :: (ForceFit a) => Parser (Expr a -> Expr a -> Expr a)
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