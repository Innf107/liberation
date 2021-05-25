{-#LANGUAGE DataKinds, ConstraintKinds, TypeFamilies, StandaloneKindSignatures, FlexibleContexts, FlexibleInstances,
   MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances, GADTs, StandaloneDeriving, TypeApplications,
   ScopedTypeVariables#-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
module Liberation.Internal.RT where

import Liberation.Internal.TypeUtil
import Liberation.Internal.Nat
import Data.Kind
import Data.Proxy
import Control.Monad
import Control.Monad.IO.Class
import Data.Type.Equality

import Unsafe.Coerce

data RT (xs :: [Type]) a where
    RTNil :: IO a -> RT '[] a
    RTCons :: (x -> RT xs a) -> RT (x : xs) a

instance Functor (RT xs) where
    fmap f (RTNil x) = RTNil $ fmap f x
    fmap f (RTCons g) = RTCons (fmap f . g)

instance Applicative (RT '[]) where
    pure x = RTNil (pure x)
    RTNil af <*> RTNil ax = RTNil (af <*> ax)

instance (Applicative (RT xs)) => Applicative (RT (x : xs)) where
    pure x = RTCons (\_ -> pure x)
    RTCons af <*> RTCons ax = RTCons (\x -> af x <*> ax x)

instance Monad (RT '[]) where
    (RTNil ma) >>= f = RTNil $ ma >>= \case
        x -> case f x of (RTNil io) -> io
            --joinRTNil $ f <$> ma

joinRTNil :: RT '[] (RT '[] a) -> RT '[] a
joinRTNil (RTNil x) = RTNil $ join $ (\(RTNil a) -> a) <$> x

instance (Monad (RT xs)) => Monad (RT (x : xs)) where
    RTCons fa >>= f = RTCons $ \x -> fa x >>= \a ->
        let (RTCons fy) = f a in fy x

instance MonadIO (RT '[]) where
    liftIO = RTNil

instance (MonadIO (RT xs)) => MonadIO (RT (x : xs)) where
    liftIO io = RTCons $ \_ -> liftIO io


class GetAt (ix :: Nat) xs where
    getAt :: RT xs (At ix xs)


instance (MonadIO (RT xs)) => GetAt Z (x : xs) where
    getAt = RTCons (\x -> pure x)

instance forall ix x xs. (GetAt ix xs) => GetAt (S ix) (x : xs) where
    getAt = RTCons (\_ -> getAt @ix)

class (GetAt (IxOf x xs) xs) => GetRT x xs where
    getRT :: RT xs x

instance (GetAt (IxOf x xs) xs) => GetRT x xs where
    getRT = case atIxOfId @x @xs of Refl -> getAt @(IxOf x xs)

atIxOfId :: forall x xs. (At (IxOf x xs) xs) :~: x
atIxOfId = unsafeCoerce Refl
-- TODO: Provide an actual proof!

run :: RT '[] a -> IO a
run (RTNil x) = x

runRT :: x -> RT (x : xs) a -> RT xs a
runRT x (RTCons f) = f x

type Has :: [[Type] -> Constraint] -> [Type] -> Constraint
type Has cs es = (MonadIO (RT es), All cs es)


