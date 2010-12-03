module TheMonad where

import Control.Monad.Identity
import Control.Monad.Error  
import Control.Monad.Reader 
import Control.Monad.State  

import Data.Traversable
import Data.Map (Map)
import qualified Data.Map as Map

import Scoping

type TheState   = ScopeState
initState = initScopeState

type TheContext = ScopeContext 
initContext = initScopeContext

type TheError   = ParseError

type TCM = StateT TheState (ReaderT TheContext (ErrorT TheError Identity))

runTCM :: TCM a -> TheState -> Either TheError (a, TheState)
runTCM k st = runIdentity $ runErrorT $ k `runStateT` st `runReaderT` initContext
