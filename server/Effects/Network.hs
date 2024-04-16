{-# LANGUAGE GADTs, FlexibleContexts, TemplateHaskell, TypeOperators, DataKinds #-}

module Effects.Network
( Network
, readMsg
, writeMsg
, initialise
, close
, runNetwork
, runNetworkHandle
) where


import Control.Monad.Freer
import Control.Monad.Freer.State
import Network.Socket hiding (close)
import System.IO


data Network r where
  Initialise :: Network Handle
  ReadMsg :: Network String
  WriteMsg :: String -> Network ()
  Close    :: Network ()

initialise :: (Member Network es) => Eff es Handle
initialise = send Initialise

readMsg :: (Member Network es) => Eff es String
readMsg = send ReadMsg

writeMsg :: (Member Network es) => String -> Eff es ()
writeMsg = send . WriteMsg

close :: (Member Network es) => Eff es ()
close = send Close

runNetwork :: LastMember IO es => Socket -> Eff (Network : es) a -> Eff es a
runNetwork sock = evalState (error "Network not initialised") . reinterpret go
  where go :: (LastMember IO es) => Network r -> Eff (State Handle : es) r
        go Initialise = do
          h <- sendM $ socketToHandle sock ReadWriteMode
          sendM $ hSetBuffering h LineBuffering
          put h
          return h
        go ReadMsg = do
          h <- get
          sendM $ stripLast <$> hGetLine h
        go (WriteMsg msg) = do
          h <- get
          sendM $ hPutStrLn h msg
        go Close = get >>= sendM . hClose

runNetworkHandle :: LastMember IO es => Handle -> Eff (Network : es) a -> Eff es a
runNetworkHandle h = interpretM go
  where go :: Network r -> IO r
        go Initialise = hSetBuffering h LineBuffering >> return h
        go ReadMsg = stripLast <$> hGetLine h
        go (WriteMsg msg) = hPutStrLn h msg
        go Close = hClose h


stripLast :: String -> String
stripLast = filter (\c -> c /= '\r' && c /= '\n')
