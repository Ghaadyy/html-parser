module DOM where

import qualified Data.Map as Map

type HTMLTag = String
type HTMLContent = String
type Attributes = Map.Map String String

data DOMTree 
    = EmptyTree 
    | HTMLElement HTMLTag Attributes [DOMTree]  
    | TextNode HTMLContent
    deriving (Show, Eq)

addElement :: DOMTree -> DOMTree -> DOMTree
addElement (HTMLElement tag attrs children) el = HTMLElement tag attrs (children ++ [el])
addElement EmptyTree el = el
addElement tree _ = tree

tagName :: DOMTree -> HTMLTag
tagName (HTMLElement tagName _ _) = tagName
tagName _ = ""