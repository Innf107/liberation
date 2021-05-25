{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts,  MultiParamTypeClasses, 
    TypeOperators, DataKinds, MonoLocalBinds, UndecidableInstances, RankNTypes, KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Liberation.Reader where  
  
import Liberation
import Liberation.Effect

data IReader r = IReader {
    _ask :: forall es. Has '[] es => RT es r
}

mkEffect ''IReader

--class Effect (IReader r) es => Reader r es where
--    ask :: RT es r
    
--instance forall r es. (MonadIO (RT es), GetRT (IReader r) es) => Reader r es where
--    ask = do
--        r <- getRT
--        _ask r


runReader :: r -> RT (IReader r : es) a -> RT es a
runReader r = runRT (IReader {_ask = pure r})

runReaderIO :: IO r -> RT (IReader r : es) a -> RT es a
runReaderIO r = runRT (IReader {_ask = liftIO r})

