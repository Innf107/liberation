{-#LANGUAGE DataKinds, ConstraintKinds, TypeFamilies, StandaloneKindSignatures, FlexibleContexts, FlexibleInstances,
   MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances, ScopedTypeVariables, GADTs, TypeApplications#-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Liberation.Internal.TypeUtil where

import Data.Kind
import Data.Type.Equality
import GHC.TypeLits (TypeError, ErrorMessage (..))
import Liberation.Internal.Nat

type At :: Nat -> [k] -> l
type family At ix xs where
    At ix '[] = TypeError (Text "At: Index is out of range by " :<>: ShowType ix)
    At Z (x : xs) = x
    At (S ix) (x : xs) = At ix xs

type IxOf :: k -> [k] -> Nat
type family IxOf x xs where
    IxOf x '[] = TypeError (Text "IxOf: List missing type " :<>: ShowType x)
    IxOf x (x : xs) = Z
    IxOf x (y : ys) = S (IxOf x ys)


type All :: [k -> Constraint] -> k -> Constraint
type family All cs x where
    All '[] k = ()
    All (c : cs) k = (c k, All cs k)

