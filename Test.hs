{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Combinators
import Grammar (html)
import DOM (DOMTree(..))
import Data.Map (fromList)

main :: IO ()
main = hspec $ do

  -- 🔍 1️⃣ **Lexer Tests**
  describe "Lexer Tests" $ do
    it "Tokenizes a simple HTML string" $ do
      let result = runParser tagParser "<html>"
      result `shouldBe` Right (Tag "html", "")

    it "Tokenizes a closing tag" $ do
      let result = runParser tagParser "</html>"
      result `shouldBe` Right (ClosingTag "html", "")

    it "Tokenizes a tag with hyphens" $ do
      let result = runParser tagParser "<custom-tag>"
      result `shouldBe` Right (Tag "custom-tag", "")

    it "Tokenizes a closing tag with hyphens" $ do
      let result = runParser tagParser "</custom-tag>"
      result `shouldBe` Right (ClosingTag "custom-tag", "")


  -- 🌲 2️⃣ **DOM Parser Tests**
  describe "DOM Parser Tests" $ do
    it "Parses a simple DOM Tree" $ do
      let input = "<html><body><h1>Welcome</h1></body></html>"
      let expected = Right 
            ( HTMLElement "html" (fromList []) 
              [ HTMLElement "body" (fromList []) 
                [ HTMLElement "h1" (fromList []) 
                  [ TextNode "Welcome" ] 
                ] 
              ]
            , "" 
            )
      runParser html input `shouldBe` expected

    it "Parses nested tags properly" $ do
      let input = "<div><p>Hello <strong>World</strong></p></div>"
      let expected = Right 
            ( HTMLElement "div" (fromList [])
              [ HTMLElement "p" (fromList [])
                [ TextNode "Hello "
                , HTMLElement "strong" (fromList []) [TextNode "World"]
                ]
              ]
            , "" 
            )
      runParser html input `shouldBe` expected

    it "Fails with unmatched tags" $ do
      let input = "<html><body><div></body></html>"
      let result = runParser html input
      result `shouldBe` Left [TagsNotMatched]


  -- 🔍 3️⃣ **DOM Operations Tests**
  describe "DOM Operations Tests" $ do

    let domTree = 
          HTMLElement "html" (fromList []) 
            [ HTMLElement "body" (fromList [])
              [ HTMLElement "div" (fromList [("id", "main"), ("class", "container")]) 
                [ TextNode "Content" ]
              , HTMLElement "p" (fromList [("class", "text-muted")]) 
                [ TextNode "A paragraph" ]
              ]
            ]

    it "Finds an element by ID" $ do
      let result = findById "main" domTree
      result `shouldBe` Just (HTMLElement "div" (fromList [("id", "main"), ("class", "container")]) [TextNode "Content"])

    it "Returns Nothing if ID is not found" $ do
      let result = findById "nonexistent" domTree
      result `shouldBe` Nothing

    it "Finds elements by class name" $ do
      let result = findByClass "container" domTree
      result `shouldBe` [HTMLElement "div" (fromList [("id", "main"), ("class", "container")]) [TextNode "Content"]]

    it "Finds elements by multiple class names" $ do
      let result = findByClass "text-muted" domTree
      result `shouldBe` [HTMLElement "p" (fromList [("class", "text-muted")]) [TextNode "A paragraph"]]

    it "Finds elements by tag name" $ do
      let result = findByTag "div" domTree
      result `shouldBe` [HTMLElement "div" (fromList [("id", "main"), ("class", "container")]) [TextNode "Content"]]

    it "Finds elements by attributes" $ do
      let result = findByAttribute "id" "main" domTree
      result `shouldBe` [HTMLElement "div" (fromList [("id", "main"), ("class", "container")]) [TextNode "Content"]]

    it "Finds multiple elements by attributes" $ do
      let result = findByAttribute "class" "text-muted" domTree
      result `shouldBe` [HTMLElement "p" (fromList [("class", "text-muted")]) [TextNode "A paragraph"]]

    it "Adds a child to a DOM tree" $ do
      let newChild = HTMLElement "footer" (fromList []) [TextNode "Footer content"]
      let updatedTree = addChild domTree newChild
      updatedTree `shouldBe` HTMLElement "html" (fromList []) 
        [ HTMLElement "body" (fromList [])
          [ HTMLElement "div" (fromList [("id", "main"), ("class", "container")]) 
            [ TextNode "Content" ]
          , HTMLElement "p" (fromList [("class", "text-muted")]) 
            [ TextNode "A paragraph" ]
          ]
        , HTMLElement "footer" (fromList []) [TextNode "Footer content"]
        ]