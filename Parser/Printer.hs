module Main where

import Tokenizer (makeToken)
import Parser hiding (main)
import Decs
import Data.List

main :: IO ()
main = do
  putStrLn "Please enter the name of your jack file (i.e. Main)"
  fileName <- getLine
  file <- readFile (fileName++".jack")
  let ast = parseString file
  makeToken fileName
  writeFile (fileName++".xml") (toXML ast)
  putStrLn "Completed Parsing, Main.xml created..."

type XML = String

sp num = take num $ cycle " "

toXML :: Jack -> XML
toXML file = "<class>\n" ++ toClass file 2 ++ "</class>\n"

tsym num s = sp num ++ "<symbol> " ++ (check s) ++ " </symbol>\n"
  where
    check s
      | s == "<" = "&lt;"
      | s == ">" = "&gt;"
      | s == "&" = "&amp;"
      | otherwise =  s

tkey num k = sp num ++ "<keyword> " ++ k ++ " </keyword>\n"
ttyp num t = sp num ++ toType t
tid  num i = sp num ++ "<identifier> " ++ i ++ " </identifier>\n"

dep depth = (replicate (2 * depth) ' ')

toClass :: Jack -> Int -> XML
toClass c num = case c of
   (Class ident decs) ->
     tkey num "class" ++
     tid num ident ++
     tsym num "{" ++
     toDecs decs num ++
     tsym num "}"

toDecs ::[Declaration] -> Int -> XML
toDecs [] _ = ""
toDecs (x:xs) num = case x of
  (ClassVarDec keyword typ varnames) ->
    sp num ++ "<classVarDec>\n" ++
    tkey num2 keyword ++
    ttyp num2 typ ++
    toClassVar varnames num2 ++
    tsym num2 ";" ++
    sp num ++ "</classVarDec>\n" ++ toDecs xs num
  (SubDec keyword typ subname params subbody) ->
    sp num ++ "<subroutineDec>\n" ++
    tkey num2 keyword ++
    ttyp num2 typ ++
    tid num2 subname ++
    tsym num2 "(" ++
    sp num2 ++ "<parameterList>\n" ++
    toParamList params num2 ++
    sp num2 ++ "</parameterList>\n" ++
    tsym num2 ")" ++
    toSubBody subbody num2 ++
    sp num ++ "</subroutineDec>\n" ++ toDecs xs num
  where
    num2 = num+2

toType :: Type -> String
toType t = case t of
  TypeKey keyword -> "<keyword> " ++ keyword ++ " </keyword>\n"
  TypeClass clas -> "<identifier> " ++ clas ++ " </identifier>\n"

toClassVar :: [VarName] -> Int -> String
toClassVar [] _ = ""
toClassVar [x] num = tid num x
toClassVar (x:xs) num = tid num x ++ tsym num "," ++ toClassVar xs num

toParamList :: [Params] -> Int -> String
toParamList [] _ = ""
toParamList [x] num = case x of
  Params (typ, varname) -> ttyp num2 typ ++ tid num2 varname
    where num2 = num+2
toParamList (x:xs) num = case x of
  Params (typ, varname) -> ttyp num2 typ ++ tid num2 varname ++ tsym num2 ","
                           ++ toParamList xs num
    where num2 = num+2

toSubBody :: SubBody -> Int -> String
toSubBody x num = case x of
  SubBodyVar vars ->
    sp num ++ "<subroutineBody>\n" ++
    tsym num2 "{" ++
    toVarDecs vars num2 ++
    tsym num2 "}" ++
    sp num  ++ "</subroutineBody>\n"
  SubBodyStatement states ->
    sp num ++ "<subroutineBody>\n" ++
    tsym num2 "{" ++
    toStatements states num ++
    tsym num2 "}" ++
    sp num  ++ "</subroutineBody>\n"
  SubBody vars states ->
    sp num ++ "<subroutineBody>\n" ++
    tsym num2 "{" ++
    toVarDecs vars num2 ++
    toStatements states num ++
    tsym num2 "}" ++
    sp num  ++ "</subroutineBody>\n"
  where num2 = num+2
        num4 = num2+2

toStatements :: [Statement] -> Int -> String
toStatements states num =
    sp num2 ++ "<statements>\n" ++
    toStatement states num2 ++
    sp num2 ++ "</statements>\n" where
      num2 = num + 2
      num4 = num2 + 2

toStatement :: [Statement] -> Int -> String
toStatement [] _ = ""
toStatement (x:xs) num = case x of
  Let key var expr ->
    sp num2 ++ "<letStatement>\n" ++
    tkey num4 key ++
    tid num4 var ++
    tsym num4 "=" ++
    toExpr expr num4 ++
    tsym num4 ";" ++
    sp num2  ++ "</letStatement>\n" ++
    toStatement xs num
  SubLet keyword var expr1 expr2 ->
    sp num2 ++ "<letStatement>\n" ++
    tkey num4 keyword ++
    tid num4 var ++
    tsym num4 "[" ++
    toExpr expr1 num4 ++
    tsym num4 "]" ++
    tsym num4 "=" ++
    toExpr expr2 num4 ++
    tsym num4 ";" ++
    sp num2 ++ "</letStatement>\n" ++
    toStatement xs num
  If key expr statements ->
    sp num2 ++ "<ifStatement>\n" ++
    tkey num4 key ++
    tsym num4 "(" ++
    toExpr expr num4 ++
    tsym num4 ")" ++
    tsym num4 "{" ++
    toStatements statements num2 ++
    tsym num4 "}" ++
    sp num2 ++ "</ifStatement>\n" ++
    toStatement xs num
  IfElse key1 expr statements1 key2 statements2 ->
    sp num2 ++ "<ifStatement>\n" ++
    tkey num4 key1 ++
    tsym num4 "(" ++
    toExpr expr num4 ++
    tsym num4 ")" ++
    tsym num4 "{" ++
    toStatements statements1 num4 ++
    tsym num4 "}" ++
    tkey num4 key2 ++
    tsym num4 "{" ++
    toStatements statements2 num4 ++
    tsym num4 "}" ++
    sp num2 ++ "</ifStatement>\n" ++
    toStatement xs num
  While key expr statements ->
    sp num2 ++ "<whileStatement>\n" ++
    tkey num4 key ++
    tsym num4 "(" ++
    toExpr expr num4 ++
    tsym num4 ")" ++
    tsym num4 "{" ++
    toStatements statements num2 ++
    tsym num4 "}" ++
    sp num2 ++ "</whileStatement>\n" ++
    toStatement xs num
  Do key subCall ->
    sp num2 ++ "<doStatement>\n" ++
    tkey num4 key ++
    toSubCall subCall num4 ++
    tsym num4 ";" ++
    sp num2 ++ "</doStatement>\n" ++
    toStatement xs num
  ReturnExp key expr ->
    sp num2 ++ "<returnStatement>\n" ++
    tkey num4 key ++
    toExpr expr num4 ++
    tsym num4 ";" ++
    sp num2 ++ "</returnStatement>\n" ++
    toStatement xs num
  Return key ->
    sp num2 ++ "<returnStatement>\n" ++
    tkey num4 key ++
    tsym num4 ";" ++
    sp num2 ++ "</returnStatement>\n" ++
    toStatement xs num
  where num2 = num+2
        num4 = num2+2

toTerm :: Term -> Int -> String
toTerm x num =
    sp num ++ "<term>\n" ++
    (case x of
      IntConst x -> sp num2 ++ "<integerConstant> " ++ show x ++ " </integerConstant>\n"
      StringConst x -> sp num2 ++ "<stringConstant> " ++ x ++ " </stringConstant>\n"
      KeywordTerm x -> tkey num2 x
      VarTerm x -> tid num2 x
      VarExpr x expr -> tid num2 x ++ tsym num2 "[" ++
                        toExpr expr num2  ++ tsym num2 "]"
      SimpleExpr expr -> tsym num2 "(" ++ toExpr expr num2 ++ tsym num2 ")"
      Unary string all@(ExprOpTerm term expr) -> tsym num2 string ++ toTerm term num2
      Subroutine sub -> toSubCall sub num2
      Symbol x -> tsym num2 x) ++
    sp num ++ "</term>\n"
  where num2 = num+2

toExpr :: Expr -> Int -> String
toExpr (ExprOpTerm term opTerms) num  =
  sp num ++ "<expression>\n" ++
  toTerm term num2 ++ toOpTerms opTerms num2 ++
  sp num ++ "</expression>\n"
  where num2 = num + 2
        num4 = num2 +2

toOpTerms :: [(String, Term)] -> Int -> String
toOpTerms [] _ = []
toOpTerms ((name,term):xs) num =
  tsym num name ++
  toTerm term num ++
  toOpTerms xs num

toSymExp [] _ = ""
toSymExp [x] num = toExpr x num
toSymExp (x:xs) num = toExpr x num ++ tsym num "," ++ toSymExp xs num

toSubCall a num =
  case a of
    SubCaller name expr -> tid num name ++
                           tsym num "(" ++
                           sp num ++ "<expressionList>\n" ++
                           toSymExp expr num2  ++
                           sp num ++ "</expressionList>\n" ++
                           tsym num ")"

    ClassCall name subname expr -> tid num name ++
                                   tsym num "." ++
                                   tid num subname ++
                                   tsym num "(" ++
                                   sp num ++ "<expressionList>\n" ++
                                   toSymExp expr num2  ++
                                   sp num ++ "</expressionList>\n" ++
                                   tsym num ")"

    VarCall name subname expr -> tid num name ++
                                 tsym num "." ++
                                 tid num subname ++
                                 tsym num "(" ++
                                 sp num ++ "<expressionList>\n" ++
                                 toSymExp expr num2  ++
                                 sp num ++ "</expressionList>\n" ++
                                 tsym num ")"
  where num2 = num+2

toVarName :: [VarName] -> Int -> String
toVarName [] _ = ""
toVarName [x] num = tid num x
toVarName (x:xs) num = tid num x ++ tsym num "," ++ toVarName xs num

toVarDecs :: [VarDec] -> Int -> String
toVarDecs [] num = ""
toVarDecs (x:xs) num  = case x of
  VarDec typ varnames ->
    sp num ++ "<varDec>\n" ++
    tkey num2 "var" ++
    ttyp num2 typ ++
    toVarName varnames num2 ++
    tsym num2 ";" ++
    sp num ++ "</varDec>\n" ++ toVarDecs xs num
  where num2 = num+2
