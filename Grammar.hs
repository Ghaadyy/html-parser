module Grammar (html) where

import qualified Data.Map as M
import Combinators
import Control.Applicative (Alternative(..))
import qualified Data.Map as Map
import DOM (DOMTree(..), tagName)
import Data.Char (isAsciiLower, isSpace, isAsciiUpper, isDigit)

html :: Parser DOMTree
html = do
    (HTMLElement openTagName attributes children) <- tagOpen
    children <- many' (textParser <|> html)
    (HTMLElement closeTagName _ _) <- tagClose
    if openTagName /= tail closeTagName then Parser $ \input -> Left [TagsNotMatched]
    else return (HTMLElement openTagName attributes children)

tags:: [String]
tags =
  [ "html", "head", "title", "body", "header", "footer", "nav", "main"
  , "section", "article", "aside", "h1", "h2", "h3", "h4", "h5", "h6"
  , "p", "ul", "ol", "li", "a", "img", "div", "span", "form", "input"
  , "button", "label", "textarea", "select", "option", "table", "tr"
  , "td", "th", "thead", "tbody", "footer", "script", "style", "link"
  , "meta", "canvas", "figure", "figcaption", "audio", "video"
  ]

tagOpen :: Parser DOMTree
tagOpen = do
  _ <- char '<'
  tagName <- choice (map string tags)
  whitespaceParser
  attrs <- attributes
  whitespaceParser
  _ <- char '>'
  return $ HTMLElement tagName attrs []

tagClose :: Parser DOMTree
tagClose = do
  _ <- string "</"
  tagName <- choice (map string tags)
  _ <- char '>'
  return $ HTMLElement ("/" ++ tagName) M.empty []

attributes :: Parser (M.Map String String)
attributes = M.fromList <$> many' attribute

attribute :: Parser (String, String)
attribute = attributeSingleQuoted <|> attributeDoubleQuoted

attributeSingleQuoted :: Parser (String, String)
attributeSingleQuoted = do
  whitespaceParser
  key <- many' (satisfy isAsciiLower)
  whitespaceParser
  char '='
  whitespaceParser
  char '\''
  value <- many' (satisfy (/= '\''))
  char '\''
  return (key, value)

attributeDoubleQuoted :: Parser (String, String)
attributeDoubleQuoted = do
  whitespaceParser
  key <- many' (satisfy isAsciiLower)
  whitespaceParser
  char '='
  whitespaceParser
  char '\"'
  value <- many' (satisfy (/= '\"'))
  char '\"'
  return (key, value)

textParser :: Parser DOMTree
textParser = do
  txt <- many1 (satisfy (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || isSpace c))
  return $ TextNode txt

whitespaceParser :: Parser Token
whitespaceParser = do
    ws <- many' (satisfy isSpace)
    return $ Whitespace ws