{-#LANGUAGE FlexibleContexts#-}
module MTL where

import Control.Monad.State.Class as S
  
mtlProgram :: (S.MonadState Int m) => m Int
mtlProgram = do
    n <- S.get
    if n <= 0
    then return n
    else S.put (n - 1) >> mtlProgram

