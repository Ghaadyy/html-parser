{-# LANGUAGE LambdaCase #-}

module Combinators where

import Control.Applicative (Alternative(..))
import Data.Char (isSpace, isAsciiLower, isAsciiUpper)
import GHC.Unicode (isDigit)
import Debug.Trace (trace)
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
    | Text String
    | Whitespace String
    | Epsilon
    deriving Show

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input -> Right (a, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left [Empty]

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ err <> err'
          consumed -> consumed
      consumed -> consumed

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \case
    [] -> trace "End of input" $ Left [EndOfInput]
    hd : rest
      | predicate hd -> Right (hd, rest) -- trace ("Consumed: " ++ [hd]) $
      | otherwise    -> Left [Unexpected] -- trace ("Unexpected: " ++ [hd]) $

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

whitespaceParser :: Parser Token
whitespaceParser = do
    ws <- many1 (satisfy isSpace)
    return $ Whitespace ws

tagOpen :: Parser DOMTree
tagOpen = do
  _ <- char '<'
  tagName <- many' (satisfy isAsciiLower)
  _ <- char '>'
  return $ HTMLElement tagName []

tagClose :: Parser DOMTree
tagClose = do
  _ <- string "</"
  tagName <- many' (satisfy isAsciiLower)
  _ <- char '>'
  return $ HTMLElement ("/" ++ tagName) []

textParser :: Parser DOMTree
textParser = do
  txt <- many1 (satisfy (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || isSpace c))
  return $ TextNode txt

-- test input
input :: String
input = "<html><ol><li>test</li><li>another item</li></ol></html>"

-- tagParser :: Parser Token
-- tagParser = tagOpen <|> tagClose

-- lexer :: Parser [Token]
-- lexer = many' (whitespaceParser <|> textParser <|> tagParser)

-- runLexer :: String -> Either [Error] [Token]
-- runLexer input = fmap fst (runParser lexer input)