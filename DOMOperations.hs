module DOMOperations where

import qualified Data.Map as Map
import Data.List (isInfixOf)
import DOM (DOMTree(..), tagName)
import Control.Applicative (Alternative(..)) 

findById :: String -> DOMTree -> Maybe DOMTree
findById _ EmptyTree = Nothing
findById _ (TextNode _) = Nothing
findById targetId el@(HTMLElement _ attrs children) 
    | Map.lookup "id" attrs == Just targetId = Just el
    | otherwise = foldr (\child acc -> acc <|> findById targetId child) Nothing children

findByClass :: String -> DOMTree -> [DOMTree]
findByClass _ EmptyTree = []
findByClass _ (TextNode _) = []
findByClass targetClass el@(HTMLElement _ attrs children) 
    | maybe False (targetClass `isInfixOf`) (Map.lookup "class" attrs) = el : childrenResults
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
    | Map.lookup key attrs == Just value = el : childrenResults
    | otherwise = childrenResults
  where
    childrenResults = concatMap (findByAttribute key value) children

addChild :: DOMTree -> DOMTree -> DOMTree
addChild (HTMLElement tag attrs children) newChild = HTMLElement tag attrs (children ++ [newChild])
addChild tree _ = tree