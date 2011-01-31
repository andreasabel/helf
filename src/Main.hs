{-# LANGUAGE TupleSections #-}

module Main where

import Control.Bifunctor

import Lexer (alexScanTokens)
import qualified Parser as HappyParser

import qualified Concrete as C
import qualified Abstract as A

import qualified Stream

import TheMonad
import qualified Scoping 
import qualified ScopeMonad as Scoping



import ErrorExpected




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
  putStrLn $ "%%% parsing %%%"
  let cdecls = HappyParser.parse t
{-
  putStrLn (show cdecls)
-}
  putStrLn $ "%%% scope checking %%%"
  (adecls, st) <- doScopeCheck cdecls
{-
--  cdecls <- doUnparse adecls st
  cdecls <- return $ runSRM (Scoping.unparse adecls) st
  putStrLn . show $ cdecls
-}
  putStrLn $ "%%% type checking %%%"
  doTypeCheck st adecls
  putStrLn $ "%%% closing " ++ show fileName ++ " %%%"

doTypeCheck :: Scoping.ScopeState -> A.Declarations -> IO ()
doTypeCheck st decls = do
  res <- runCheckDecls decls 
  case res of
    Left err -> do 
      putStrLn $ "error during typechecking:\n" ++  err
      exitFailure
    Right () -> return ()
{-
    Right (edecls, st) -> 
      return (edecls, signature st)
-}

doScopeCheck :: C.Declarations -> IO (A.Declarations, TheState)
doScopeCheck cdecls = case runTCM (Scoping.parse cdecls) initState of
     Left err          -> do 
       putStrLn $ "scope check error: " ++ show err
       exitFailure
     Right (adecls, st) -> return (adecls, st)

data StList s e a 
  = Fail e
  | Done s
  | Cons a (StList s e a)

build :: (s -> Either e (Either s' (a, s))) -> s -> StList s' e a
build f s = loop (f s) where
  loop result = case result of
    Left err             -> Fail err
    Right (Left s')      -> Done s'
    Right (Right (a, s)) -> Cons a (loop (f s))

scopeCheck :: C.Declarations -> StList TheState Scoping.ParseError [A.Declaration]
scopeCheck (C.Declarations cdecls) = build step (cdecls, initState) where
  step :: ([C.Declaration], TheState) -> Either Scoping.ParseError (Either TheState ([A.Declaration], ([C.Declaration], TheState)))
  step ([]  , st) = Right $ Left st
  step (d:ds, st) = bimap id (Right . bimap id (ds,)) $ runTCM (Scoping.parse d) st
{-
scopeCheckStream :: 
scopeCheckStream cdecls = foldr 
  (\ cdecl (adecls, st) -> 
     let (ds, st') = runTCM (Scoping.parse cdecl) st
     in (ds
-}
{-
scopeCheckStream :: C.Declarations -> Stream TCM A.Declaration
scopeCheckStream (C.Declarations cdecls) = loop cdecls initState where
  loop [] st = Stream.empty
  loop (cdec:cdecls) = 
-}

{-
doUnparse :: A.Declarations -> TheState -> IO C.Declarations
doUnparse adecls st = case runTCM (Scoping.unparse adecls) st of
     Left err          -> do 
       putStrLn $ "error during unparsing: " ++ show err
       exitFailure
     Right (cdecls, _) -> return cdecls
-}
