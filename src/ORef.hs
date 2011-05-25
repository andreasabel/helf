{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Ordered References.
--
--   Adds an extra 'Integer' to an 'IORef' in order to store refs in a 'Map'.
module ORef where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.IORef

-- * Ordered References

data ORef a = ORef { refId :: Integer, theRef :: IORef a }

instance Eq (ORef a) where
  r == r' = theRef r == theRef r'

instance Ord (ORef a) where
  r <= r' = refId r <= refId r'

instance Show (ORef a) where
  show r = "r" ++ show (refId r)

-- * A monad to manipulate ORefs

class (Functor m, Applicative m, MonadIO m) => MonadORef m where

  newORef    :: a -> m (ORef a)

  readORef   :: ORef a -> m a

  writeORef  :: ORef a -> a -> m a
  writeORef r a = modifyORef r (const a)

  modifyORef :: ORef a -> (a -> a) -> m a
  modifyORef r f = do
    a <- readORef r
    writeORef r (f a)

  assign     :: ORef a -> ORef a -> m (ORef a)
  assign r r' = do
    writeORef r =<< readORef r'
    return r

-- * monad liftings    

instance MonadORef m => MonadORef (ReaderT r m) where
  newORef a     = lift $ newORef a
  readORef r    = lift $ readORef r
  writeORef r a = lift $ writeORef r a

instance MonadORef m => MonadORef (StateT s m) where
  newORef a     = lift $ newORef a
  readORef r    = lift $ readORef r
  writeORef r a = lift $ writeORef r a
{-
  newORef a     = StateT $ \ s -> do r <- newORef a; return (r, s)
  readORef r    = StateT $ \ s -> do a <- readORef r; return (a, s)
  writeORef r a = StateT $ \ s -> do a <- writeORef r a; return (a, s)
-}

instance (Error e, MonadORef m) => MonadORef (ErrorT e m) where
  newORef a     = lift $ newORef a    
  readORef r    = lift $ readORef r   
  writeORef r a = lift $ writeORef r a

-- * concrete ORef monad

-- we hide the state monad!
newtype ORefM a = ORefM { runORefM :: StateT Integer IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

next :: (Enum a, MonadState a m) => m a
next = do
  st <- get
  put $ succ st
  return st

instance MonadORef ORefM where

  newORef a = do
    i <- ORefM $ next
    r <- liftIO $ newIORef a
    return $ ORef { refId = i, theRef = r }
    
  readORef r = liftIO $ readIORef $ theRef r

  writeORef r a = do 
    liftIO $ writeIORef (theRef r) a
    return a

evalORef :: ORefM a -> IO a
evalORef (ORefM cont) = evalStateT cont 0

--  modifyORef r f = do liftIO $ modifyIORef (theRef r) f

{-
instance Functor ORefM where
  fmap f ma = ma >>= \ a -> return (f a)
-}
  