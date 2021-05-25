{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts,  MultiParamTypeClasses,
    TypeOperators, DataKinds, MonoLocalBinds, UndecidableInstances, RankNTypes #-}
module Liberation.State where

import Control.Monad.IO.Class
import Liberation.Internal.RT

import Data.IORef
import Control.Concurrent.MVar

data RState s = RState {
    _get    :: forall es. Has '[] es => RT es s
,   _put    :: forall es. Has '[] es => s -> RT es ()
,   _modify :: forall es. Has '[] es => (s -> s) -> RT es ()
}

class State s es where
    get :: RT es s
    put :: s -> RT es ()
    modify :: (s -> s) -> RT es ()

instance forall r es. (MonadIO (RT es), GetRT (RState r) es) => State r es where
    get = do
        r <- getRT
        _get r
    put s = do
        r <- getRT
        _put r s
    modify f = do
        r <- getRT
        _modify r f

evalStateIORef :: (MonadIO (RT es)) => s -> RT (RState s : es) a -> RT es a
evalStateIORef s rt = do
    ref <- liftIO $ newIORef s
    runRT (RState {
        _get    = liftIO $ readIORef ref
    ,   _put    = liftIO . writeIORef ref
    ,   _modify = liftIO . modifyIORef ref
    }) rt

runStateIORef :: (MonadIO (RT es)) => s -> RT (RState s : es) a -> RT es (s, a)
runStateIORef s rt = do
    ref <- liftIO $ newIORef s
    a <- runRT (RState {
            _get    = liftIO $ readIORef ref
        ,   _put    = liftIO . writeIORef ref
        ,   _modify = liftIO . modifyIORef ref
        }) rt
    rs <- liftIO $ readIORef ref
    pure (rs, a)

execStateIORef :: (MonadIO (RT es)) => s -> RT (RState s : es) a -> RT es s
execStateIORef s rt = fst <$> runStateIORef s rt

runStateMVar :: (MonadIO (RT es)) => s -> RT (RState s : es) a -> RT es (s, a)
runStateMVar s rt = do
    mv <- liftIO $ newMVar s
    a <- runRT (RState {
        _get    = liftIO $ readMVar mv
    ,   _put    = \x -> liftIO $ modifyMVar_ mv (\_ -> pure x) 
    ,   _modify = \f -> liftIO $ modifyMVar_ mv (\x -> pure (f x)) 
    }) rt
    rs <- liftIO $ readMVar mv
    pure (rs, a)


