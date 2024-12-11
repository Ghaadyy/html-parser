module DOM where

import qualified Data.Map as M
import Data.List (intercalate)

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

type ParentPos = Int
type Pos = Int
data Patch
    = ReplaceNode Pos DOMTree
    | UpdateAttributes Pos HTMLAttributes
    | InsertNode ParentPos Pos DOMTree
    | RemoveNode ParentPos Pos
    deriving (Show, Eq)

-- React's diffing algorithm

-- If the HTMLElements are of different types, Replace entire subtree
-- If both elements are the same, check the attributes.
-- If attributes are the same, check the inner content.

-- For more info, check out this article from the React team.
-- https://legacy.reactjs.org/docs/reconciliation.html

diff :: DOMTree -> DOMTree -> [Patch]
diff = diff' 0

diff' :: Int -> DOMTree -> DOMTree -> [Patch]
diff' _ EmptyTree EmptyTree = []
diff' idx (TextNode tl) r@(TextNode tr)
    | tl == tr = [] -- same text content, no change
    | otherwise = [ReplaceNode idx r] -- different, replace content with right.
diff' idx (HTMLElement ltag lattrs lchild) r@(HTMLElement rtag rattrs rchild)
    | ltag /= rtag = [ReplaceNode idx r] -- replace the entire tree
    | lattrs /= rattrs = UpdateAttributes idx (diffAttributes lattrs rattrs) : diffChildren idx 0 lchild rchild -- different attrs, check the children too for changes
    | otherwise = diffChildren idx 0 lchild rchild -- no changes above, check children
diff' idx _ new = [ReplaceNode idx new] -- catchall

diffChildren :: Int -> Int -> [DOMTree] -> [DOMTree] -> [Patch]
diffChildren _ _ [] []                      = []
diffChildren parentIdx idx (x:xs) []        = RemoveNode parentIdx idx : diffChildren parentIdx (idx + 1) xs []
diffChildren parentIdx idx [] (y:ys)        = InsertNode parentIdx idx y : diffChildren parentIdx (idx + 1) [] ys
diffChildren parentIdx idx (x:xs) (y:ys)    = diff' idx x y ++ diffChildren parentIdx (idx + 1) xs ys

diffAttributes :: HTMLAttributes -> HTMLAttributes -> HTMLAttributes
diffAttributes old new = M.union new (M.difference new old)

displayDiff :: [Patch] -> String
displayDiff patches = unlines $ map formatPatch patches

formatTree :: DOMTree -> String
formatTree = formatTree' 0

formatTree' :: Int -> DOMTree -> String
formatTree' level (TextNode content) = indent level ++ "TextNode \"" ++ content ++ "\""
formatTree' level (HTMLElement tag attrs children) = indent level ++ "HTMLElement \"" ++ tag ++ "\" " ++ formatAttributes attrs ++ " with children:\n" ++ unlines (map ((indent level ++ ) . formatTree' (level + 1)) children)
formatTree' level EmptyTree =  indent level ++ "EmptyTree"

formatPatch :: Patch -> String
formatPatch (ReplaceNode idx newTree) = "~ Replace node at index " ++ show idx ++ " with:\n" ++ formatTree newTree
formatPatch (UpdateAttributes idx newAttrs) = "~ Update attributes at index " ++ show idx ++ ":\n" ++ formatAttributes newAttrs
formatPatch (InsertNode parentIdx childIdx newTree) = "+ Insert node at parent index " ++ show parentIdx ++ ", child index " ++ show childIdx ++ ":\n" ++ formatTree newTree
formatPatch (RemoveNode parentIdx childIdx) = "- Remove node at parent index " ++ show parentIdx ++ ", child index " ++ show childIdx

formatAttributes :: HTMLAttributes -> String
formatAttributes attrs =
    unlines [concat ["  ", k, "=\"", v, "\""] | (k, v) <- M.toList attrs]

indent :: Int -> String
indent level = concat $ replicate level "   "