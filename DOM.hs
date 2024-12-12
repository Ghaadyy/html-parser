module DOM (
    DOMTree(..),
    Patch(..),
    addElement,
    addChild,
    findById,
    findByClass,
    findByTag,
    findByAttribute,
    diff,
    displayDiff,
    toMarkdown
) where

import qualified Data.Map as M
import Data.List (intercalate, isInfixOf)
import Data.Maybe (fromMaybe)
import Control.Applicative (Alternative(..)) 

type HTMLAttributes = M.Map String String
type HTMLTag = String
type HTMLContent = String

data DOMTree 
    = EmptyTree 
    | HTMLElement HTMLTag HTMLAttributes [DOMTree]  
    | TextNode HTMLContent
    deriving (Show, Eq)

addElement :: DOMTree -> DOMTree -> DOMTree
addElement (HTMLElement tag attrs children) el = HTMLElement tag attrs (children ++ [el])
addElement EmptyTree el = el
addElement tree _ = tree

findById :: String -> DOMTree -> Maybe DOMTree
findById _ EmptyTree = Nothing
findById _ (TextNode _) = Nothing
findById targetId el@(HTMLElement _ attrs children) 
    | M.lookup "id" attrs == Just targetId = Just el
    | otherwise = foldr (\child acc -> acc <|> findById targetId child) Nothing children

findByClass :: String -> DOMTree -> [DOMTree]
findByClass _ EmptyTree = []
findByClass _ (TextNode _) = []
findByClass targetClass el@(HTMLElement _ attrs children) 
    | maybe False (targetClass `isInfixOf`) (M.lookup "class" attrs) = el : childrenResults
    | otherwise = childrenResults
  where
    childrenResults = concatMap (findByClass targetClass) children

findByTag :: String -> DOMTree -> [DOMTree]
findByTag _ EmptyTree = []
findByTag _ (TextNode _) = []
findByTag targetTag el@(HTMLElement tag _ children) 
    | tag == targetTag = el : childrenResults
    | otherwise = childrenResults
  where
    childrenResults = concatMap (findByTag targetTag) children

findByAttribute :: String -> String -> DOMTree -> [DOMTree]
findByAttribute _ _ EmptyTree = []
findByAttribute _ _ (TextNode _) = []
findByAttribute key value el@(HTMLElement _ attrs children) 
    | M.lookup key attrs == Just value = el : childrenResults
    | otherwise = childrenResults
  where
    childrenResults = concatMap (findByAttribute key value) children

addChild :: DOMTree -> DOMTree -> DOMTree
addChild (HTMLElement tag attrs children) newChild = HTMLElement tag attrs (children ++ [newChild])
addChild tree _ = tree

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


toMarkdown :: DOMTree -> String
toMarkdown EmptyTree = []
toMarkdown (TextNode content) = content ++ " "
toMarkdown (HTMLElement "h1" _ children) = "# " ++ concatMap toMarkdown children ++ "\n"
toMarkdown (HTMLElement "h2" _ children) = "## " ++ concatMap toMarkdown children ++ "\n"
toMarkdown (HTMLElement "h3" _ children) = "### " ++ concatMap toMarkdown children ++ "\n"
toMarkdown (HTMLElement "h4" _ children) = "#### " ++ concatMap toMarkdown children ++ "\n"
toMarkdown (HTMLElement "h5" _ children) = "##### " ++ concatMap toMarkdown children ++ "\n"
toMarkdown (HTMLElement "h6" _ children) = "###### " ++ concatMap toMarkdown children ++ "\n"
toMarkdown (HTMLElement "ol" _ children) = toMdOrderedList 1 children
    where
        toMdOrderedList :: Int -> [DOMTree] -> String
        toMdOrderedList _ [] = "\n"
        toMdOrderedList n (li@(HTMLElement "li" _ children):xs) = show n ++ ". " ++ concatMap toMarkdown children ++ "\n" ++ toMdOrderedList (n + 1) xs
        toMdOrderedList n (_:xs) = toMdOrderedList n xs
toMarkdown (HTMLElement "ul" _ children) = toMdUnorderedList children
    where
        toMdUnorderedList :: [DOMTree] -> String
        toMdUnorderedList [] = "\n"
        toMdUnorderedList (li@(HTMLElement "li" _ children):xs) = "* " ++ concatMap toMarkdown children ++ "\n" ++ toMdUnorderedList xs
        toMdUnorderedList (x:xs) = toMdUnorderedList xs
toMarkdown (HTMLElement "a" attributes children) =
    maybe (concatMap toMarkdown children) -- gets called if result is Nothing
          (\href -> "[" ++ concatMap toMarkdown children ++ "](" ++ href ++ ") ")  -- gets called if result is Just ...
          (M.lookup "href" attributes >>= \href -> if null href then Nothing else Just href) -- result
toMarkdown (HTMLElement "img" attributes _) =
    maybe "" -- return empty if cant find src attribute aka Nothing
          (\src ->
              let altText = M.lookup "alt" attributes
                  title = M.lookup "title" attributes
              in "![" ++ fromMaybe "" altText ++ "](" ++ src ++ fromMaybe "" title ++ ") ") -- altText and title are replaced by empty if they fail
          (M.lookup "src" attributes) -- result
toMarkdown (HTMLElement _ _ children) = concatMap toMarkdown children ++ " " -- other unsupported