module Grammar where

import Combinators
import Control.Applicative (Alternative(..))
import qualified Data.Map as Map
import DOM (DOMTree(HTMLElement), tagName)

html :: Parser DOMTree
html = do
    (HTMLElement openTagName attributes children) <- tagOpen
    children <- many' (textParser <|> html)
    (HTMLElement closeTagName _ _) <- tagClose
    if openTagName /= tail closeTagName then Parser $ \input -> Left [TagsNotMatched]
    else return (HTMLElement openTagName attributes children)
-- this does not *properly* take into account the matching of tags yet
-- we should handle error propagation and aggregation properly.