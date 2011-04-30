{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.Error	( MonadError(..) )
-- import Control.Bifunctor

import qualified Data.List as List

import Lexer (alexScanTokens)
import qualified Parser as HappyParser

import qualified Concrete as C
import qualified Abstract as A

import qualified Stream

import TheMonad
import qualified Scoping 
import qualified ScopeMonad as Scoping

-- import ErrorExpected

-- Engines
import qualified OrderedCom2 as Ordered
import qualified Closures
import qualified Monolith
import qualified HerBruijn
import qualified NamedExplSubst

import System
import System.IO (stdout, hSetBuffering, BufferMode(..))
import System.Console.GetOpt (getOpt, usageInfo, ArgOrder(Permute,ReturnInOrder)
			     , OptDescr(..), ArgDescr(..)
			     )

data Engine = Closures | Ordered | HerBruijn | Monolith
  deriving (Eq,Show,Enum,Bounded,Read)

engines :: [Engine]
engines = [minBound..maxBound]

defaultEngine = Closures

{-
data Options = Options
  { optEngine    :: Engine
--  , optInputFile :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { optEngine    = defaultEngine
--  , optInputFile = Nothing
  }

{- | @f :: Flag opts@  is an action on the option record that results from
     parsing an option.  @f opts@ produces either an error message or an
     updated options record
-}
type Flag opts = opts -> opts -- Either String opts
-}
{-
inputFlag :: FilePath -> Flag Options
inputFlag f o =
    case optInputFile o of
	Nothing  -> return $ o { optInputFile = Just f }
	Just _	 -> throwError "only one input file allowed"
-}

data Flag = Engine Engine
          deriving Show

engineFlag :: String -> Flag
engineFlag arg = 
  let engine :: Engine
      engine = read arg
  in  Engine engine

options :: [OptDescr Flag]
options = 
  [ Option ['e'] ["engine"] (ReqArg engineFlag "ENGINE") 
      ("set lambda engine (default = " ++ show defaultEngine ++ "), possible values: " ++ show engines)
  ] 

{-

engineFlag :: String -> Flag Options
engineFlag arg o = 
  let engine :: Engine
      engine = read arg
  in o { optEngine = engine }



options :: [OptDescr (Flag Options)]
options = 
  [ Option ['e'] ["engine"] (ReqArg engineFlag "ENGINE") 
      ("set lambda engine (default = " ++ show defaultEngine ++ "), possible values: " ++ show engines)
  ] 
-- | Don't export
parseOptions' ::
  [String] -> [OptDescr (Flag opts)] -> (String -> Flag opts) -> Flag opts
parseOptions' argv opts = \defaults ->
    case getOpt (Permute) opts argv of
	(o,_,[])    -> foldl (>>=) (return defaults) o
	(_,_,errs)  -> throwError $ concat errs


-- | Parse the standard options.
parseOptions :: [String] -> Either String Options
parseOptions argv =
  -- checkOpts =<<
    parseOptions' argv options inputFlag defaultOptions
-}

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "HELF - Haskell implementation of the Edinburgh Logical Framework"
  putStrLn $ "(C) Andreas Abel and Nicolai Kraus"
  args <- getArgs
  (o,files) <- case getOpt Permute options args of
                 (o,files,[]) -> return (o,files)
                 (_,_,errs)   -> do
                   putStrLn ("error during parsing commandline:" ++ show errs)
                   exitFailure
  let isEngine (Engine _) = True
      isEngine (_)        = False
  let engine = case List.find isEngine o of
               Nothing -> defaultEngine
               Just (Engine e) -> e
  mapM_ (mainFile engine) files

mainFile :: Engine -> String -> IO ()
mainFile engine fileName = do
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
  putStrLn $ "%%% type checking with engine " ++ show engine ++ " %%%"
  doTypeCheck engine st adecls
  putStrLn $ "%%% closing " ++ show fileName ++ " %%%"

runCheckDecls Closures = Closures.runCheckDecls
runCheckDecls Ordered  = Ordered.runCheckDecls
runCheckDecls HerBruijn = HerBruijn.runCheckDecls
runCheckDecls Monolith = Monolith.runCheckDecls

doTypeCheck :: Engine -> Scoping.ScopeState -> A.Declarations -> IO ()
doTypeCheck engine st decls = do
  res <- runCheckDecls engine decls 
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

{- an unfinished attempt to use streams

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
-}

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
