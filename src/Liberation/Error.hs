{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts,  MultiParamTypeClasses,
    TypeOperators, DataKinds, MonoLocalBinds, UndecidableInstances, RankNTypes, GADTs #-}

module Liberation.Error where

import Liberation
import Liberation.Effect
import Data.Typeable

import Control.Exception hiding (throw)

data RError e = RError {
    _throw :: forall a es. Has '[] es => e -> RT es a
}

class Error e es where
    throw :: forall a. e -> RT es a

instance forall r es. (MonadIO (RT es), GetRT (RError r) es) => Error r es where
    throw e = do
        rt <- getRT
        _throw rt e


runErrorIOException :: (Exception e) => RT (RError e : es) a -> RT es a
runErrorIOException = runRT RError {
        _throw = liftIO . throwIO
    }

data LiberationErrorException e = LiberationErrorException e deriving (Eq)

instance Show (LiberationErrorException e) where
    show _ = "<LiberationErrorException>"

instance Typeable e => Exception (LiberationErrorException e)


runError :: (Typeable e, CatchRT es, Has '[] es) => RT (RError e : es) a -> RT es (Either e a)
runError rt = (Right <$> runRT RError {
        _throw = \e -> liftIO $ throwIO (LiberationErrorException e)
    } rt)
   `catchRT` (\(LiberationErrorException e) -> pure $ Left e)
-- TODO: Probably Needs MonadUnliftIO or MonadBaseControl

class CatchRT es where
    catchRT :: (Exception e) => RT es a -> (e -> RT es a) -> RT es a

instance CatchRT '[] where
    catchRT (RTNil io) h = RTNil $ io `catch` (\x -> case h x of RTNil y -> y)

instance (CatchRT es) => CatchRT (e : es) where
    catchRT (RTCons f) h = RTCons (\x -> f x `catchRT` (\e -> case h e of RTCons hf -> hf x))

mapError :: forall e1 e2 a es. (Has '[Error e2] es, CatchRT es, Typeable e1) => (e1 -> e2) -> RT (RError e1 : es) a -> RT es a
mapError f rt = (runRT RError {
               _throw = \e -> liftIO $ throwIO (LiberationErrorException e)
           } rt)
          `catchRT` (\(LiberationErrorException e) -> throw (f e))

fromEither :: (Has '[Error e] es) => Either e a -> RT es a
fromEither (Left e) = throw e
fromEither (Right x) = pure x


note :: (Has '[Error e] es) => e -> Maybe a -> RT es a
note e Nothing  = throw e
note _ (Just x) = pure x
