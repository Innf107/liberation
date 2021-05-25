{-#LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses#-}
module RIOClass where

import Control.Monad.Reader
import Data.IORef

data Env = Env {
    intState :: IORef Int
}

class HasState s r where
    getS :: ReaderT r IO s
    putS :: s -> ReaderT r IO ()

instance HasState Int Env where
    getS = asks intState >>= liftIO . readIORef
    putS x = asks intState >>= liftIO . flip writeIORef x


rioClassProgram :: (HasState Int r) => ReaderT r IO Int
rioClassProgram = do
    n <- getS
    if n <= 0
    then return n
    else putS (n - 1) >> rioClassProgram
