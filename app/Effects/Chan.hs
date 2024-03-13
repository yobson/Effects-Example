{-# LANGUAGE GADTs, FlexibleContexts, TemplateHaskell, TypeOperators, DataKinds #-}

module Effects.Chan
( Chan
, broadcast
, readChan
, dupChan
, runChan
) where

import Control.Monad.Fix
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Concurrent.STM

type Name = String

data Chan r where
  Broadcast :: Name -> String -> Chan ()
  ReadChan  :: Name -> Chan String
  DupChan   :: Chan (TChan (String,String))
makeEffect ''Chan

runChan :: LastMember IO es => TChan (Name, String) -> Eff (Chan : es) a -> Eff es a
runChan chan = interpretM go
  where go :: Chan r -> IO r
        go (Broadcast name msg) = atomically $ writeTChan chan (name, msg)
        go (ReadChan name) = fix $ \loop -> do
          (n,msg) <- atomically $ readTChan chan
          if n == name then loop
                       else return msg
        go DupChan = atomically $ dupTChan chan 
