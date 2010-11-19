module Main where

import Lexer
import Parser

import Common
import qualified Concrete as C
{-
import qualified Abstract as A
import ScopeChecker
import TypeCheck
import Closures
-}

import System
import System.IO (stdout, hSetBuffering, BufferMode(..))


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "HELF - Haskell implementation of the Edinburgh Logical Framework"
  putStrLn $ "(C) Andreas Abel and Nicolai Kraus"
  args <- getArgs
  mapM_ mainFile args

mainFile :: String -> IO ()
mainFile fileName = do
  putStrLn $ "%%% opening " ++ show fileName ++ " %%%"
  file <- readFile fileName
--  putStrLn "%%% lexing %%%"
  let t = alexScanTokens file 
--  putStrLn (show t)
  putStrLn "%%% parsing %%%"
  let ast =  parse t
  putStrLn (show ast)
{-
  putStrLn "%%% scope checking %%%"
  ast2 <- doScopeCheck ast
  putStrLn "%%% type checking %%%"
  doTypeCheck ast2
-}
  putStrLn $ "%%% closing " ++ show fileName ++ " %%%"

{-
doTypeCheck :: [A.Declaration] -> IO ([A.EDeclaration], Signature)
doTypeCheck decls = do 
  k <- typeCheck decls
  case k of
    Left err -> do 
      putStrLn $ "error during typechecking:\n" ++ show err
      exitFailure
    Right (edecls, st) -> 
      return (edecls, signature st)

doScopeCheck :: [C.Declaration] -> IO [A.Declaration]
doScopeCheck decl = case scopeCheck decl of
     Left err -> do putStrLn $ "scope check error: " ++ show err
                    exitFailure
     Right (decl',_) -> return $ decl'
-}
