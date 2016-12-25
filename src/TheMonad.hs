module TheMonad where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Traversable
import Data.Map (Map)
import qualified Data.Map as Map

import Scoping (ParseError)
import ScopeMonad

type TheState   = ScopeState
initState = initScopeState

type TheContext = ScopeContext
initContext = initScopeContext

type TheError   = ParseError

type TCM = StateT TheState (ReaderT TheContext (ExceptT TheError Identity))

runTCM :: TCM a -> TheState -> Either TheError (a, TheState)
runTCM k st = runIdentity $ runExceptT $ k `runStateT` st `runReaderT` initContext

type SRM = Reader TheState

runSRM :: SRM a  -> TheState -> a
runSRM k st = runReader k st
