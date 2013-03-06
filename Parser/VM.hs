module Main where
import Tokenizer (makeToken)
import Parser hiding (main)
import Decs
import Data.List
import Control.Monad.Reader

main = do
  putStrLn "Please enter the name of your jack file (i.e. Main)"
  fileName <- getLine
  file <- readFile (fileName++".jack")
  let ast = parseString file
  makeToken fileName
  writeFile (fileName++".xml") (toVM ast)
  putStrLn "Completed Parsing, Main.xml created..."

type VM = String

toVM :: Jack -> VM
toVM file = toClass file

toClass :: Jack -> VM
toClass c = case c of
   (Class ident decs) ->
     toDecs decs

toDecs :: [Declaration] -> VM
toDecs [] _ = ""
toDecs (x:xs) = case x of
  (ClassVarDec keyword typ varnames) ->
    toClassVar varnames (case x of
      IntConst x -> 
      StringConst x ->
      KeywordTerm x -> 
      VarTerm x -> 
      VarExpr x expr -> toExpr expr 
      SimpleExpr expr -> toExpr expr
      Unary string all@(ExprOpTerm term expr) -> toTerm term 
      Subroutine sub -> toSubCall sub
      Symbol x -> toSym x) ++

toExpr :: Expr -> String
toExpr (ExprOpTerm term opTerms) =
  toTerm term ++ toOpTerms opTerms ++

toOpTerms :: [(String, Term)] -> String
toOpTerms [] _ = []
toOpTerms ((name,term):xs) =
  toTerm term num ++
  toOpTerms xs num

toSymExp [] _ = ""
toSymExp [x] = toExpr x
toSymExp (x:xs) = toExpr x ++ toSymExp xs num

toSubCall a =
  case a of
    SubCaller name expr -> toSymExp expr ++
    ClassCall name subname expr -> toSymExp expr  ++
    VarCall name subname expr -> toSymExp expr num2  ++