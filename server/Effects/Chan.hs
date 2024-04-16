{-# LANGUAGE GADTs, FlexibleContexts, TemplateHaskell, TypeOperators, DataKinds #-}

module Effects.Chan
( Chan
, broadcast
, readChan
, dupChan
, runChan
, addName
, removeName
, getNames
, nameHandle
) where

import Control.Monad.Fix
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Concurrent.STM

type Name = String

data Chan r where
  Broadcast  :: Name -> String -> Chan ()
  ReadChan   :: Name -> Chan String
  DupChan    :: Chan (TChan (String,String))
  AddName    :: String -> Chan ()
  RemoveName :: String -> Chan ()
  GetNames   :: Chan [String]
  NameHandle :: Chan (TVar [String])
makeEffect ''Chan

runChan :: LastMember IO es => TVar [String] -> TChan (Name, String) -> Eff (Chan : es) a -> Eff es a
runChan names chan = interpretM go
  where go :: Chan r -> IO r
        go (Broadcast name msg) = atomically $ writeTChan chan (name, msg)
        go (ReadChan name) = fix $ \loop -> do
          (n,msg) <- atomically $ readTChan chan
          if n == name then loop
                       else return msg
        go DupChan = atomically $ dupTChan chan 
        go (AddName s)    = atomically $ modifyTVar names (s:)
        go (RemoveName s) = atomically $ modifyTVar names $ filter (/= s) 
        go (GetNames)     = atomically $ readTVar names
        go (NameHandle)   = return names
