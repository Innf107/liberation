{-#LANGUAGE DataKinds, ConstraintKinds, TypeFamilies, StandaloneKindSignatures, FlexibleContexts, FlexibleInstances,
   MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances, GADTs, StandaloneDeriving, TypeApplications,
   ScopedTypeVariables#-}
module Liberation.Internal.Nat where

import GHC.TypeLits (TypeError, ErrorMessage (..))
  
data {-kind-} Nat = Z | S Nat
  
type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
  
data SNat (n :: Nat) where
    SZ :: SNat Z
    SS :: SNat n -> SNat (S n)

type (+) :: Nat -> Nat -> Nat
type family a + b where
    Z + b = b
    (S a) + b = a + S b

type (-) :: Nat -> Nat -> Nat
type family a - b where
    a - Z = a
    Z - (S b) = TypeError (Text "(-): Cannot create negative Nats (would create: -" :<>: ShowType (S b) :<>: Text ")")
    (S a) - (S b) = a - b

class MkSNat (n :: Nat) where
    mkSNat :: SNat n
    
instance MkSNat Z where
    mkSNat = SZ
      
instance (MkSNat n) => MkSNat (S n) where
    mkSNat = SS (mkSNat)
