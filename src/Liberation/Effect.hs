{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Liberation.Effect
    ( runRT
    , GetRT(..)
    , MonadIO(..)
    , Effect
    )where
  
import Liberation.Internal.RT ( runRT, GetRT(..), GetAt )
import Control.Monad.IO.Class ( MonadIO (..) )
import Liberation.Internal.TypeUtil ( IxOf )
import Liberation.Internal.Effect ( Effect )
