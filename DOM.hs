module DOM where

import qualified Data.Map as M

type HTMLAttributes = M.Map String String
type HTMLTag = String
type HTMLContent = String

data DOMTree = EmptyTree 
    | HTMLElement HTMLTag HTMLAttributes [DOMTree]  
    | TextNode HTMLContent
    deriving (Show, Eq)

addElement :: DOMTree -> DOMTree -> DOMTree
addElement tree el = EmptyTree

tagName :: DOMTree -> HTMLTag
tagName (HTMLElement tagName@(head:tail) _ _)  
    | head == '/' = tail
    | otherwise = tagName