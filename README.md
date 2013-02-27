
Jaskell
================================

A lexical analyzer of the Jack language
---------------------------------------
This project will read in a .jack file, analyze its structure, and print a hierarchy tree
along with a list of tokens in XML format. One for the tokens, one for the code hierarchy.
Stay tuned for the eventual creation of VM commands. All specs are based on the nand2tetris
Hack platform.

Instructions
---------------------------------------
```bash
ghc Printer.hs -o parser
./parser
Please enter the name of your jack file (i.e. Main)
Main
Completed Lexing, MainT.xml created...
Completed Parsing, Main.xml created...
```
