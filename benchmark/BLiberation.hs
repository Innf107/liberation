{-#LANGUAGE DataKinds, FlexibleContexts#-}
module BLiberation where

import Liberation
import Liberation.State
  
program :: (Has '[State Int] es) => RT es Int
program = do
    n <- get
    if n <= 0
    then return n
    else put (n - 1) >> program
