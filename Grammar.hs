module Grammar where

import Combinators
import Control.Applicative (Alternative(..))
import Debug.Trace
import DOM (DOMTree(HTMLElement), tagName)

-- Basic HTML Grammar
-- html     := tagOpen (text|html)? tagClose | epsilon
-- text     := ...
-- tagOpen  := ...
-- tagClose := ...

html :: Parser DOMTree
html = do
    tagTree <- tagOpen
    children <- many' (textParser <|> html)
    tagCloseTree <- tagClose
    if tagName tagTree /= tagName tagCloseTree
        then Parser $ \input -> Left [TagsNotMatched]
    else return (HTMLElement (tagName tagTree) children)
-- this does not *properly* take into account the matching of tags yet
-- we should handle error propagation and aggregation properly.