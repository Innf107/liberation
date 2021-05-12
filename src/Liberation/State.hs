{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts,  MultiParamTypeClasses,
    TypeOperators, DataKinds, MonoLocalBinds, UndecidableInstances #-}
module Liberation.State where

import Control.Monad.IO.Class
import Liberation.Internal.RT

import Data.IORef
import Control.Concurrent.MVar

data RState s = RState {
    _get :: IO s
,   _put :: s -> IO ()
,   _modify :: (s -> s) -> IO ()
}

class State s es where
    get :: RT es s
    put :: s -> RT es ()
    modify :: (s -> s) -> RT es ()

instance forall r es. (MonadIO (RT es), GetRT (RState r) es) => State r es where
    get = do
        r <- getRT
        liftIO (_get r)
    put s = do
        r <- getRT
        liftIO (_put r s)
    modify f = do
        r <- getRT
        liftIO (_modify r f)

evalStateIORef :: (MonadIO (RT es)) => s -> RT (RState s : es) a -> RT es a
evalStateIORef s rt = do
    ref <- liftIO $ newIORef s
    runRT (RState {
        _get = readIORef ref
    ,   _put = writeIORef ref
    ,   _modify = modifyIORef ref
    }) rt

runStateIORef :: (MonadIO (RT es)) => s -> RT (RState s : es) a -> RT es (s, a)
runStateIORef s rt = do
    ref <- liftIO $ newIORef s
    a <- runRT (RState {
            _get = readIORef ref
        ,   _put = writeIORef ref
        ,   _modify = modifyIORef ref
        }) rt
    rs <- liftIO $ readIORef ref
    pure (rs, a)

execStateIORef :: (MonadIO (RT es)) => s -> RT (RState s : es) a -> RT es s
execStateIORef s rt = fst <$> runStateIORef s rt

runStateMVar :: (MonadIO (RT es)) => s -> RT (RState s : es) a -> RT es (s, a)
runStateMVar s rt = do
    mv <- liftIO $ newMVar s
    a <- runRT (RState {
        _get = readMVar mv
    ,   _put = \x -> modifyMVar_ mv (\_ -> pure x) 
    ,   _modify = \f -> modifyMVar_ mv (\x -> pure (f x)) 
    }) rt
    rs <- liftIO $ readMVar mv
    pure (rs, a)


