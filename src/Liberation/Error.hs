{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts,  MultiParamTypeClasses,
    TypeOperators, DataKinds, MonoLocalBinds, UndecidableInstances, RankNTypes #-}

module Liberation.Error where

import Liberation
import Liberation.Effect
import Data.Typeable

import Control.Exception hiding (throw)

data RError e = RError {
    _throw :: forall a. e -> IO a
}

class Error e es where
    throw :: forall a. e -> RT es a

instance forall r es. (MonadIO (RT es), GetRT (RError r) es) => Error r es where
    throw e = do
        rt <- getRT
        liftIO (_throw rt e)


runErrorIOException :: (Exception e) => RT (RError e : es) a -> RT es a
runErrorIOException = runRT RError {
        _throw = throwIO
    }

data LiberationErrorException e = LiberationErrorException e deriving (Eq)

instance Show (LiberationErrorException e) where
    show _ = "<LiberationErrorException>"

instance Typeable e => Exception (LiberationErrorException e)


runError :: (Typeable e) => RT (RError e : es) a -> RT es (Either e a)
runError rt = Right <$> runRT RError {
        _throw = \e -> throwIO (LiberationErrorException e)
    } rt
-- `catch` (\LiberationErrorException e -> Left e)
-- TODO: Probably Needs MonadUnliftIO or MonadBaseControl

{-
mapError :: (Has '[Error e2] es) => (e1 -> e2) -> RT (RError e1 : es) a -> RT es a
mapError f = runRT RError {
        _throw = throw . f
    }
TODO: Not quite possible?
-}

fromEither :: (Has '[Error e] es) => Either e a -> RT es a
fromEither (Left e) = throw e
fromEither (Right x) = pure x


note :: (Has '[Error e] es) => e -> Maybe a -> RT es a
note e Nothing  = throw e
note _ (Just x) = pure x
