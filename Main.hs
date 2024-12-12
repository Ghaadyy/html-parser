module Main where

import DOM (DOMTree(..), diff, toMarkdown, displayDiff)
import Combinators (Parser(..), Error)
import Grammar (html)

parseExample :: String -> Either [Error] (DOMTree, String)
parseExample = runParser html

main :: IO ()
main = do
    let input = "<html>\
                    \<body>\
                        \<h1>Welcome</h1>\
                        \<h5>Hello world</h5>\
                        \<p>This is an example paragraph</p>\
                        \<ul>\
                            \<li>Item 1</li>\
                            \<li>Item 2</li>\
                        \</ul>\
                        \<ol>\
                            \<li>Item 1</li>\
                            \<li>Item 2</li>\
                            \<li>Item 3</li>\
                            \<li>Item 4</li>\
                            \<li>Item 5</li>\
                        \</ol>\
                        \<img alt='description' title = 'Just example' src='example'></img>\
                        \<ol>\
                            \<li><a href='example1'>Item 1</a></li>\
                            \<li>Item 2</li>\
                        \</ol>\
                        \<a href='example2'>Item 1</a>\
                    \</body>\
                \</html>"

    let input2 = "<html>\
                    \<body>\
                        \<h1>Testing diff checking</h1>\
                        \<h5>Hello world</h5>\
                        \<p>This is an example paragraph</p>\
                        \<ul>\
                            \<li>Item 1</li>\
                        \</ul>\
                        \<ol>\
                            \<li>Item 1</li>\
                            \<li>Item 2</li>\
                            \<li>Item 3</li>\
                            \<li>Item 4</li>\
                            \<li>Item 5</li>\
                        \</ol>\
                        \<img alt='new description' title = 'Just example' src='example'></img>\
                        \<ol>\
                            \<li><a href='example1'>Item 1</a></li>\
                            \<li>Item 2</li>\
                        \</ol>\
                        \<a href='example2'>Item 1</a>\
                        \<p>new item here</p>\
                    \</body>\
                \</html>"
                
    let result = parseExample input
    case result of
        Left errors -> do
            mapM_ print errors
        Right (tree1, _) -> do
            let result2 = parseExample input2
            case result2 of
                Left err2 -> do
                    mapM_ print err2
                Right (tree2, _) -> do
                    putStrLn "Diff computation\n"
                    putStrLn $ displayDiff $ diff tree1 tree2
                    putStrLn "Markdown Generation\n"
                    putStrLn $ toMarkdown tree1