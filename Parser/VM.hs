module Main where

import Tokenizer (makeToken)
import Parser hiding (main)
import Decs
import SymbolTable
import Data.List
import Data.Char
import Control.Monad.Reader 
import Control.Monad.Writer
import Debug.Trace

main = do
  putStrLn "Please enter the name of your jack file (i.e. Main)"
  fileName <- getLine
  file <- readFile (fileName++".jack")
  writeFile (fileName++".xml")  (runReader toClass $ parseString file)
  putStrLn $  "Completed Parsing, " ++ fileName ++ ".xml created..."

toClass = do { env <- ask; case env of Class ident decs -> return $ toDecs decs env }

toDecs [] = return ""
toDecs ((SubDec keyword typ subname params subbody):xs) = do
  env <- ask
  funcName <- return $ "function " ++ getClass env ++ 
              "." ++ subname ++ " " ++ (show $ getFieldCount subname env) ++ "\n"
  lcl <- return $ (case keyword of
                     "constructor" -> "push constant " ++ getClassVarCount env ++ "\n"
                     "method" -> "push argument 0\npop pointer 0\n"
                     otherwise -> "")
  res <- (case keyword of
            "constructor" -> do return "call Memory.alloc 1\npop pointer 0\n"
            otherwise -> do return "")
  let x = funcName ++ lcl ++ res
  return $ x ++ toSubBody subbody subname subname  env ++ toDecs xs env

toDecs (_:xs) = do { env <- ask; return $ toDecs xs env }

toSubBody (SubBodyStatement states) subname subtype  = do { env <- ask; return $ toStatement "0" "0" subtype states subname env }
toSubBody (SubBody _ states) subname subtype = do { env <- ask; return $ toStatement "0" "0" subtype states subname env }

inc x = show $ (read x ::Int)+1

toStatement _ _ _ [] _ = return ""
toStatement num numif subtype (x:xs) subname = do
  env <- ask
  case x of
    Let key var e -> do
           let (scope, n) = getSymNum var subname env
               exp = "pop " ++ scope ++ " " ++ n ++ "\n"
           return $ toExpr subtype e env ++ exp ++ toStatement num numif subtype xs subname env
    SubLet keyword var expr1 expr2 -> do
           let h = let (scope, name) = getSymNum var subtype env in "push " ++ scope ++ " " ++ name ++ "\n"
               j = "pop temp 0\npop pointer 1\npush temp 0\npop that 0\n"
           return $ toExpr subtype expr1 env ++ h ++ "add\n" ++ toExpr subtype expr2 env ++ j ++ toStatement num numif subtype xs subname env
    If key expr statements -> do
         let ifs = "if-goto IF_TRUE" ++ numif ++ "\ngoto IF_FALSE" ++ numif ++  "\nlabel IF_TRUE" ++ numif ++ "\n"
             ife = "label IF_FALSE" ++ numif ++ "\n"
         return $  toExpr subtype expr env ++ ifs ++
                toStatement num (inc numif) subtype statements subname env ++ ife ++
                toStatement num (inc numif) subtype xs subname env
    IfElse key1 expr statements1 key2 statements2 -> do
         return $ toExpr subtype expr env ++ 
                "if-goto IF_TRUE" ++ numif ++ "\n" ++
                "goto IF_FALSE" ++numif ++ "\n" ++
                "label IF_TRUE" ++numif ++ "\n" ++
                toStatement num (inc numif) subtype statements1 subname env ++ 
                "goto IF_END" ++numif ++ "\n"++ 
                "label IF_FALSE" ++numif ++ "\n" ++
                toStatement num (inc numif) subtype statements2 subname env ++  
                "label IF_END" ++ numif ++ "\n" ++
                toStatement num (inc numif) subtype xs subname env
    While key expr statements -> do
         let whist = "label WHILE_EXP" ++ num ++ "\n" 
             whien = "if-goto WHILE_END" ++ num ++ "\n" 
             whies = "goto WHILE_EXP"  ++ num ++ "\nlabel WHILE_END"  ++ num ++ "\n"
         return $ whist ++ toExpr subtype expr env ++ "not\n" ++ whien ++ toStatement num numif subtype statements subname env 
                    ++ whies ++toStatement (inc num) numif subtype xs subname env 
    Do key subcall -> do
         return $ toSubCall subtype subcall env ++ "pop temp 0\n" ++ toStatement num numif subtype xs subname env 
    ReturnExp key expr -> do
         return $ toExpr subtype expr env ++toStatement num numif subtype xs subname env ++ "return\n"
    Return key -> do
         return $ toStatement num numif subtype xs subname env ++ "push constant 0\nreturn\n"

isNeg x = if x < 0 then "push constant " ++ (show . abs) x ++ "\nneg\n" else "push constant " ++ show x ++ "\n"

toTerm subtype x = do 
  env <- ask
  result <- return $ case x of
              IntConst x ->  isNeg x
              StringConst x ->  "push constant " ++ (show $ length x) ++ 
                               "\ncall String.new 1\n" ++ toStr x
              KeywordTerm x -> case x of
                                 "this" -> "push pointer 0\n"
                                 "false" -> "push constant 0\n"
                                 "null" -> "push constant 0\n"
                                 "true" -> "push constant 0\nnot\n"
                                 otherwise -> x
              VarTerm x ->  let (scope, name) = getSymNum x subtype env 
                                in  "push " ++ scope ++ " " ++ name ++ "\n"
              VarExpr x expr -> let (scope, name) = getSymNum x subtype env 
                                in  toExpr subtype expr env ++ "push " ++ scope ++ " " ++ name ++ "\n" ++ "add\npop pointer 1\npush that 0\n"
              SimpleExpr expr ->  toExpr subtype expr env
              Unary string all@(ExprOpTerm term expr) -> toTerm subtype term env ++ "not\n"
              Subroutine sub ->  toSubCall subtype sub env 
              Symbol x -> traceShow (toSym x) $ toSym x ++ "\n"
  return result

toExpr subtype (ExprOpTerm term opTerms) = do { env <- ask; return $ toTerm subtype term env ++ toOpTerms subtype opTerms env}

toOpTerms subtype [] = return []
toOpTerms subtype ((name,term):xs) = do { env <- ask; return $ toTerm subtype term env ++ 
                                                 toTerm subtype (Symbol name) env ++
                                                 toOpTerms subtype xs env }
toSymExp subtype [] = return []
toSymExp subtype [x] = do { env <- ask; return $ toExpr subtype  x env }
toSymExp subtype (x:xs) = do { env <- ask; return $ toExpr subtype x env ++ toSymExp subtype xs env }

toSubCall subtype call = do
  env <- ask
  case call of
    SubCaller subname expr -> do -- do moveSquare(); 
                        className <- return $ getClass env
                        paramcount <- return $ (case (getFuncType env subname)  == "method" of
                                        True -> show $ (getParamCount subname env)+1
                                        False -> show $ getParamCount subname env)
                        let x  = "call " ++ className ++ "." ++ subname ++ " " ++ paramcount ++ "\n"
                        return $ toSymExp subtype expr env ++ x

    ClassCall name subname expr -> do --Keyboard.readInt() or square.moveDown()
                           xpr <- return $ case (isUpper $ head name) of
                                    True -> case (getClass env == name) of
                                              --own class func
                                              True -> "call " ++ name ++ "." ++ subname ++ " " ++ show (getParamCount subname env) ++ "\n"
                                               --other class func
                                              False -> "call " ++ name ++ "." ++ subname ++ " " ++ (show $ length expr) ++ "\n"
                                    False -> case (lookup name $ getClassMap env) of
                                               --other class method
                                               Just (TypeClass cname, typ, num) -> "call " ++ cname ++ "." ++ 
                                                                                   subname ++ " " ++ (show $ (length expr)+1 ) ++ "\n"
                           return $ toSymExp subtype expr env ++ xpr


                           -- xpr <- case (lookup subname $ getSubDecMap env) of
                           --                   Just x -> do  className <- return $ getClass env
                           --                                 paramcount <- return $ (case getFuncType env subname == "method" of
                           --                                                           True -> show $ (getParamCount subname env)+1
                           --                                                           False -> show $ getParamCount subname env)
                           --                                 let d  = "call " ++ getClass env ++ "." ++ subname ++ " " ++ paramcount ++ "\n"
                           --                                 return $ d ++ toSymExp subtype expr env                              
                           --                   Nothing -> case lookup name $ getClassMap env of -- method/func from other class
                           --                                Just (TypeClass cname, typ, num) ->
                           --                                    return $ "call " ++ cname ++ "." ++ 
                           --                                    subname ++ " " ++ (show $ (+1) $ length expr)
                           --                                Nothing -> return $ "call " ++ name ++ "." ++ 
                           --                                           subname ++ " " ++ (show $ length $ expr) ++ "\n"
                           -- return $ toSymExp subtype expr env ++ xpr







