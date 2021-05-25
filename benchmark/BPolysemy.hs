{-#LANGUAGE GADTs, FlexibleContexts#-}
module BPolysemy where

import qualified Polysemy as P
import qualified Polysemy.State as P

polysemyProgram :: (P.Member (P.State Int) r) => P.Sem r Int
polysemyProgram = do
    n <- P.get
    if n <= 0
    then return n
    else P.put (n - 1) >> polysemyProgram
