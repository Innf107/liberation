{-# LANGUAGE ConstraintKinds, FlexibleContexts#-}

module Liberation.Internal.Effect where

import Liberation.Internal.TypeUtil
import Liberation.Internal.RT


type Effect impl es = (GetAt (IxOf impl es) es)
