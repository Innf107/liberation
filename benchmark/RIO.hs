module RIO where

import Control.Monad.Reader
import Data.IORef
  
getR :: ReaderT (IORef a) IO a
getR = ask >>= liftIO . readIORef

putR :: a -> ReaderT (IORef a) IO ()
putR x = ask >>= liftIO . flip writeIORef x
  
rioProgram :: ReaderT (IORef Int) IO Int
rioProgram = do
    n <- getR
    if n <= 0
    then return n
    else putR (n - 1) >> rioProgram
