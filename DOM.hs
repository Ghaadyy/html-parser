module DOM where

-- import qualified Data.Map as Map

-- type HTMLAttributes = Map.Map String String
type HTMLTag = String
type HTMLContent = String

data DOMTree = EmptyTree 
    | HTMLElement HTMLTag [DOMTree]  
    | TextNode HTMLContent
    deriving (Show, Eq)

addElement :: DOMTree -> DOMTree -> DOMTree
addElement tree el = EmptyTree

tagName :: DOMTree -> HTMLTag
tagName (HTMLElement tagName@(head:tail) _)  
    | head == '/' = tail
    | otherwise = tagName