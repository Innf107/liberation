{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts,  MultiParamTypeClasses,
    TypeOperators, DataKinds, MonoLocalBinds, UndecidableInstances, RankNTypes, GADTs #-}
{-#LANGUAGE TemplateHaskell, KindSignatures#-}
module Liberation.Error where

import Liberation
import Liberation.Effect
import Data.Typeable

import Control.Monad.IO.Unlift

import Control.Exception hiding (throw)

data IError e = IError {
    _throw :: forall a es. Has '[] es => e -> RT es a
}

mkEffect ''IError

--class Effect (IError e) es => Error e es where
--    throw :: forall a. e -> RT es a

--instance forall r es. (MonadIO (RT es), GetRT (IError r) es) => Error r es where
--    throw e = do
--        rt <- getRT
--        _throw rt e


runErrorIOException :: (Exception e) => RT (IError e : es) a -> RT es a
runErrorIOException = runRT IError {
        _throw = liftIO . throwIO
    }

data LiberationErrorException e = LiberationErrorException e deriving (Eq)

instance Show (LiberationErrorException e) where
    show _ = "<LiberationErrorException>"

instance Typeable e => Exception (LiberationErrorException e)


runError :: (Typeable e, Has '[] es) => RT (IError e : es) a -> RT es (Either e a)
runError rt = withRunInIO $ \rio -> rio (Right <$> runRT IError {
        _throw = \e -> liftIO $ throwIO (LiberationErrorException e)
    } rt)
   `catch` (\(LiberationErrorException e) -> pure $ Left e)


mapError :: forall e1 e2 a es. (Has '[Error e2] es, Typeable e1) => (e1 -> e2) -> RT (IError e1 : es) a -> RT es a
mapError f rt = withRunInIO $ \rio -> rio (runRT IError {
               _throw = \e -> liftIO $ throwIO (LiberationErrorException e)
           } rt)
          `catch` (\(LiberationErrorException e) -> rio $ throw (f e))

fromEither :: (Has '[Error e] es) => Either e a -> RT es a
fromEither (Left e) = throw e
fromEither (Right x) = pure x


note :: (Has '[Error e] es) => e -> Maybe a -> RT es a
note e Nothing  = throw e
note _ (Just x) = pure x
