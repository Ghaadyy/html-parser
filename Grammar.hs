module Grammar where

import Combinators
import Control.Applicative (Alternative(..))
import qualified Data.Map as Map
import DOM (DOMTree(HTMLElement), tagName)

html :: Parser DOMTree
html = do
  tagTree <- tagOpen
  children <- many' (textParser <|> html)
  tagCloseTree <- tagClose
  if tagName tagTree /= tagName tagCloseTree
    then Parser $ \input -> Left [TagsNotMatched]
    else return (HTMLElement (tagName tagTree) Map.empty children)

tagOpen :: Parser DOMTree
tagOpen = do
  token <- tagParser
  case token of
    Tag tagName -> return $ HTMLElement tagName Map.empty []
    _ -> Parser $ \_ -> Left [TagsNotMatched]

tagClose :: Parser DOMTree
tagClose = do
  token <- tagParser
  case token of
    ClosingTag tagName -> return $ HTMLElement tagName Map.empty []
    _ -> Parser $ \_ -> Left [TagsNotMatched]