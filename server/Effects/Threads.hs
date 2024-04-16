{-# LANGUAGE GADTs, FlexibleContexts, TemplateHaskell, TypeOperators, DataKinds #-}

module Effects.Threads 
( Threads
, forkEff
, killT
, runThreads
) where

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Concurrent

data Threads r where
  ForkEff    :: IO () -> Threads ThreadId
  KillT      :: ThreadId -> Threads ()
makeEffect ''Threads


runThreads :: (LastMember IO es) => Eff (Threads : es) a -> Eff es a
runThreads = interpretM go
  where go :: Threads r -> IO r
        go (ForkEff f)    = forkIO f
        go (KillT t)      = killThread t
