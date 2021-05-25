{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts,  MultiParamTypeClasses,
    TypeOperators, DataKinds, MonoLocalBinds, UndecidableInstances, RankNTypes #-}
{-#LANGUAGE TemplateHaskell, KindSignatures#-}
module Liberation.State where

import Liberation
import Liberation.Effect

import Data.IORef
import Control.Monad.IO.Class
import Control.Concurrent.MVar

data IState s = IState {
    _get    :: forall es. Has '[] es => RT es s
,   _put    :: forall es. Has '[] es => s -> RT es ()
,   _modify :: forall es. Has '[] es => (s -> s) -> RT es ()
}

mkEffect ''IState

{-
class Effect (RState s) es => State s es where
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
-}
evalStateIORef :: forall s a es. (MonadIO (RT es)) => s -> RT (IState s : es) a -> RT es a
evalStateIORef s rt = do
    ref <- liftIO $ newIORef s
    runRT (IState {
        _get    = liftIO $ readIORef ref
    ,   _put    = liftIO . writeIORef ref
    ,   _modify = liftIO . modifyIORef ref
    }) rt

runStateIORef :: forall s a es. (MonadIO (RT es)) => s -> RT (IState s : es) a -> RT es (s, a)
runStateIORef s rt = do
    ref <- liftIO $ newIORef s
    a <- runRT (IState {
            _get    = liftIO $ readIORef ref
        ,   _put    = liftIO . writeIORef ref
        ,   _modify = liftIO . modifyIORef ref
        }) rt
    rs <- liftIO $ readIORef ref
    pure (rs, a)

execStateIORef :: forall s a es. (MonadIO (RT es)) => s -> RT (IState s : es) a -> RT es s
execStateIORef s rt = fst <$> runStateIORef s rt

runStateMVar :: forall s a es. (MonadIO (RT es)) => s -> RT (IState s : es) a -> RT es (s, a)
runStateMVar s rt = do
    mv <- liftIO $ newMVar s
    a <- runRT (IState {
        _get    = liftIO $ readMVar mv
    ,   _put    = \x -> liftIO $ modifyMVar_ mv (\_ -> pure x)
    ,   _modify = \f -> liftIO $ modifyMVar_ mv (\x -> pure (f x))
    }) rt
    rs <- liftIO $ readMVar mv
    pure (rs, a)


