{-# LANGUAGE DataKinds, TypeApplications, FlexibleContexts #-}
import Liberation
import Liberation.State
import Liberation.Error
import Liberation.Reader

import Control.Monad

import Liberation.Internal.RT
import Liberation.Internal.TypeUtil

main :: IO ()
main = print =<< run (runStateIORef @Int 5 $ runError @String $ runReader 'B' test3)

test3 :: (Has '[State Int, Error String, Reader Char] es) => RT es Int
test3 = do
  x <- test2
  c <- ask
  if (c == 'A')
  then pure 42
  else pure x

test2 :: (Has '[State Int, Error String] es) => RT es Int
test2 = runReader True test

test :: Has '[State Int, Error String, Reader Bool] es => RT es Int
test = do
    x <- get @Int
    c <- ask
    when c $ put (x * 2)
    x' <- get
    when (x' > 10) $ throw "Too large"
    put (x' * 4)
    pure x'

