{-#LANGUAGE DataKinds, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GADTs#-}
module Main where

import Liberation
import Liberation.State

import Criterion.Main

import Control.Monad.Reader
import qualified Control.Monad.State.Class as S
import Control.Monad.State (runState)

import Data.IORef

import qualified Polysemy as P
import qualified Polysemy.State as P

import qualified Baseline as B
import qualified BLiberation as L
import qualified RIO as R
import qualified RIOClass as RC
import qualified MTL as M
import qualified BPolysemy as PB

main :: IO ()
main = defaultMain [
        bench "baseline"            (whnf baseCountdown           100000)
    ,   bench "baseline multi"      (whnf multiBaseCountdown      100000)
    ,   bench "rio"                 (nfIO (rioCountDown           100000))
    ,   bench "rio multi"           (nfIO (multiRioCountDown      100000))
    ,   bench "rio-class"           (nfIO (rioClassCountDown      100000))
    ,   bench "rio-class multi"     (nfIO (multiRioClassCountDown 100000))
    ,   bench "mtl"                 (whnf mtlCountDown            100000)
    ,   bench "mtl multi"           (whnf multiMtlCountDown       100000)
    ,   bench "polysemy"            (whnf polysemyCountDown       100000)
    ,   bench "polysemy multi"      (whnf multiPolysemyCountDown  100000)
    ,   bench "liberation"          (nfIO (countDown              100000))
    ,   bench "liberation multi"    (nfIO (multiCountDown         100000))
    ]

countDown :: Int -> IO (Int, Int)
countDown initial = run $ runStateIORef initial program

multiCountDown :: Int -> IO (Int, Int)
multiCountDown initial = run $ runStateIORef initial L.program


program :: (Has '[State Int] es) => RT es Int
program = do
    n <- get
    if n <= 0
    then return n
    else put (n - 1) >> program

baseCountdown :: Int -> (Int, Int)
baseCountdown initial = baseProgram initial

multiBaseCountdown :: Int -> (Int, Int)
multiBaseCountdown initial = B.baseProgram initial

baseProgram :: Int -> (Int, Int)
baseProgram n = if n <= 0
                then (n, n)
                else baseProgram (n - 1)


rioCountDown :: Int -> IO Int
rioCountDown initial = do
    s <- newIORef initial
    runReaderT rioProgram s

multiRioCountDown :: Int -> IO Int
multiRioCountDown initial = do
    s <- newIORef initial
    runReaderT R.rioProgram s

getR :: ReaderT (IORef a) IO a
getR = ask >>= liftIO . readIORef

putR :: a -> ReaderT (IORef a) IO ()
putR x = ask >>= liftIO . flip writeIORef x

rioProgram :: ReaderT (IORef Int) IO Int
rioProgram = do
    n <- getR
    if n <= 0
    then return n
    else putR (n - 1) >> rioProgram


mtlCountDown :: Int -> (Int, Int)
mtlCountDown initial = runState mtlProgram initial

multiMtlCountDown :: Int -> (Int, Int)
multiMtlCountDown initial = runState M.mtlProgram initial


mtlProgram :: (S.MonadState Int m) => m Int
mtlProgram = do
    n <- S.get
    if n <= 0
    then return n
    else S.put (n - 1) >> mtlProgram

data Env = Env {
    intState :: IORef Int
}

class HasState s r where
    getS :: ReaderT r IO s
    putS :: s -> ReaderT r IO ()

instance HasState Int Env where
    getS = asks intState >>= liftIO . readIORef
    putS x = asks intState >>= liftIO . flip writeIORef x

rioClassCountDown :: Int -> IO Int
rioClassCountDown initial = do
    s <- Env <$> newIORef initial
    runReaderT rioClassProgram s

multiRioClassCountDown :: Int -> IO Int
multiRioClassCountDown initial = do
    s <- RC.Env <$> newIORef initial
    runReaderT RC.rioClassProgram s


rioClassProgram :: (HasState Int r) => ReaderT r IO Int
rioClassProgram = do
    n <- getS
    if n <= 0
    then return n
    else putS (n - 1) >> rioClassProgram


polysemyCountDown:: Int -> (Int, Int)
polysemyCountDown initial = P.run $ P.runState initial polysemyProgram

multiPolysemyCountDown:: Int -> (Int, Int)
multiPolysemyCountDown initial = P.run $ P.runState initial PB.polysemyProgram

polysemyProgram :: (P.Member (P.State Int) r) => P.Sem r Int
polysemyProgram = do
    n <- P.get
    if n <= 0
    then return n
    else P.put (n - 1) >> polysemyProgram
