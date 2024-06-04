module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char  -- Importing necessary functions

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- 2.1. / 3.2.
failParser :: Parser a 
failParser = Parser $ \s -> Nothing

charParser :: Char -> Parser Char
charParser c = Parser $ \s ->
  case s of 
    [] -> Nothing
    (x:xs) -> if x == c then Just (c, xs) else Nothing

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s ->
  case s of 
    [] -> Nothing
    (x:xs) -> if p x then Just (x, xs) else Nothing

instance Monad Parser where
  mp >>= f = Parser $ \s ->
    case parse mp s of 
      Nothing -> Nothing
      Just (val, rest) -> parse (f val) rest

  return x = Parser $ \s -> Just (x, s)

instance Applicative Parser where
  af <*> mp = do 
    f <- af
    v <- mp
    return $ f v

  pure = return

instance Functor Parser where 
  fmap f mp = do 
    x <- mp
    return $ f x

instance Alternative Parser where
  empty = failParser
  p1 <|> p2 = Parser $ \s -> case parse p1 s of
                                Nothing -> parse p2 s
                                x -> x

plusParser :: Parser a -> Parser [a]
plusParser p = do
  x <- p
  xs <- starParser p
  return $ x:xs


starParser :: Parser a -> Parser [a]
starParser p = plusParser p <|> pure []

whitespaceParser :: Parser String
whitespaceParser = starParser (charParser ' ')

parseVar :: Parser String
parseVar = do
  x <- predicateParser isAlpha
  xs <- starParser (predicateParser isAlphaNum)
  return (x:xs)

parseAbs :: Parser Lambda
parseAbs = do
  _ <- charParser '\\'
  whitespaceParser
  var <- parseVar
  whitespaceParser
  _ <- charParser '.'
  whitespaceParser
  expr <- parseLambdaExpr
  return (Abs var expr)

parseApp :: Parser Lambda
parseApp = do
  _ <- charParser '('
  whitespaceParser
  e1 <- parseLambdaExpr
  whitespaceParser
  e2 <- parseLambdaExpr
  whitespaceParser
  _ <- charParser ')'
  return (App e1 e2)

parseMacro :: Parser Lambda
parseMacro = do
  var <- some (predicateParser isUpper <|> predicateParser isDigit) 
  return (Macro var)

parseLambdaExpr :: Parser Lambda
parseLambdaExpr = parseAbs <|> parseApp <|> parseMacro <|> (Var <$> parseVar)

parseLambda :: String -> Lambda
parseLambda s = 
  case parse parseLambdaExpr s of
    Just (expr, "") -> expr
    _ -> error "error"

-- 3.3.
parseBinding :: Parser Line
parseBinding = do
  var <- parseVar
  whitespaceParser
  _ <- charParser '='
  whitespaceParser
  expr <- parseLambdaExpr
  return $ Binding var expr

parseEval :: Parser Line
parseEval = do
  expr <- parseLambdaExpr
  return $ Eval expr
  
parseLine :: String -> Either String Line
parseLine s = 
  case parse parseBinding s of
    Just (binding, "") -> Right binding
    _ -> case parse parseEval s of
           Just (eval, "") -> Right eval
           _ -> Left "error"

