# Functional HTML Parser

## Getting Started

To execute the demo project, run the following command.

```bash
ghc Main.hs -outputdir build -o build/main && ./build/main # executes the Main.hs
```

To run the tests, execute the following command. Make sure to have `Hspec` installed.

```bash
ghc Test.hs -outputdir build -o build/test && ./build/test # executes the Test.hs
```

## Project Structure

- `Grammar.hs`: contains the HTML grammar written with parser combinators
- `DOM.hs`: contains the DOM ADT and functions that manipulate it
- `Combinators.hs`: contains the implementation of parser combinators based on [1,2]

## Core Features

- HTML document parsing
- Diff computation between two `DOMTree`s
- Markdown compilation
- DOM-like API, featuring `findById`, `findByClass` and others
- Unit testing

## References

- [1] Daan Leijen and Erik Meijer. Parsec: Direct Style Monadic Parser Combinators
  For The Real World. 12 2001.
- [2] Heitor Toledo Lassarote de Paula. Parser Combinators in Haskell. Online, 2021.
