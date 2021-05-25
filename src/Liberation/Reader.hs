{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts,  MultiParamTypeClasses, 
    TypeOperators, DataKinds, MonoLocalBinds, UndecidableInstances, RankNTypes #-}
module Liberation.Reader where  
  
import Liberation
import Liberation.Effect
  
import Liberation.Internal.RT
import Liberation.Internal.TypeUtil
  
data RReader r = RReader {
    _ask :: forall es. Has '[] es => RT es r
}

class Effect (RReader r) es => Reader r es where
    ask :: RT es r
    
instance forall r es. (MonadIO (RT es), GetRT (RReader r) es) => Reader r es where
    ask = do
        r <- getRT
        _ask r

runReader :: r -> RT (RReader r : es) a -> RT es a
runReader r = runRT (RReader {_ask = pure r})

runReaderIO :: IO r -> RT (RReader r : es) a -> RT es a
runReaderIO r = runRT (RReader {_ask = liftIO r})

