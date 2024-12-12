{-# LANGUAGE LambdaCase #-}

module Combinators where

import qualified Data.Map as M
import Control.Applicative (Alternative(..))
import Data.Char (isSpace, isAsciiLower, isAsciiUpper)
import GHC.Unicode (isDigit)
import DOM (DOMTree(HTMLElement, EmptyTree, TextNode))

newtype Parser a = Parser {
  runParser :: String -> Either [Error] (a, String)
}

data Error
  = EndOfInput
  | Unexpected
  | Empty
  | TagsNotMatched
  deriving (Eq, Show)

data Token
  = Tag String       
  | ClosingTag String 
  | Text String
  | Whitespace String
  | Epsilon
  deriving (Eq, Show)

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)
  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad Parser where
  return = pure
  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance MonadFail Parser where
  fail :: String -> Parser a
  fail _ = Parser $ \input -> Left [Unexpected]

instance Alternative Parser where
  empty = Parser $ \_ -> Left [Empty]
  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ err <> err'
          consumed -> consumed
      consumed -> consumed

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \case
  [] -> Left [EndOfInput]
  hd : rest
    | predicate hd -> Right (hd, rest)
    | otherwise    -> Left [Unexpected]

char :: Char -> Parser Char
char a = satisfy (== a)

string :: [Char] -> Parser [Char]
string [] = return []
string (x : xs) = do
  y <- char x
  ys <- string xs
  return (y : ys)

many' :: Parser a -> Parser [a]
many' p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many1 p <|> return []
  return (x:xs)

choice :: [Parser a] -> Parser a
choice = foldr (<|>) (fail "No parser")