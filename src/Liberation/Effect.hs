module Liberation.Effect 
    ( runRT
    , GetRT(..)
    , MonadIO(..)
    )where
  
import Liberation.Internal.RT (runRT, GetRT(..))
import Control.Monad.IO.Class (MonadIO (..))  
