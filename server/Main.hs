{-# LANGUAGE DataKinds, FlexibleContexts, RecordWildCards, TypeOperators #-}

module Main where

import Network.Run.TCP
import Control.Monad.Fix
import Control.Concurrent.STM
import Options.Applicative

import Effects

data Options = Options
  { host :: String
  , port :: Int
  , logOpt :: LogOptions
  }

data LogOptions = On | Silent | File FilePath

logOptions :: Parser LogOptions
logOptions =  pure On
          <|> flag' Silent (long "silent" <> help "Don't log to command line")
          <|> File <$> strOption
                ( long "logfile"
               <> help "Log to file"
               <> metavar "FILEPATH"
                )

options :: Parser Options
options = Options
  <$> strOption
      ( long "host"
     <> metavar "IP"
     <> help "IP to start server on"
     <> showDefault
     <> value "127.0.0.1"
      )
  <*> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "Port to bind"
     <> showDefault
     <> value 4242
      )
  <*> logOptions

main :: IO ()
main = execParser opts >>= mainOpt
  where opts = info (options <**> helper)
          ( fullDesc
         <> progDesc "Simple Haskell Chat Server"
         <> header "Example - a simple chat server"
          )

runLogger' :: (LastMember IO es) => LogOptions -> Eff (Logger : es) a -> Eff es a
runLogger' On = runLogger
runLogger' Silent = runSilentLogger
runLogger' (File fp) = runFileLogger fp

mainOpt :: Options -> IO ()
mainOpt Options{..} = do
  putStrLn $ "Server started on " <> host <> ":" <> show port
  t <- newBroadcastTChanIO
  runTCPServer (Just host) (show port) $ \s -> runM $ runThreads $ runLogger' logOpt $ runNetwork s $ runChan t runConn

runConn :: Eff [Chan, Network, Logger, Threads, IO] ()
runConn = do
  h <- initialise

  putLog Info "New Connection"
  writeMsg "Hi! What's your name?"
  name <- readMsg
  writeMsg $ "Welcome " <> name
  writeMsg "Type 'quit' to exit"
  broadcast name $ "--> " <> name <> " entered chat." 
  putLog Info $ name <> " entered chat"

  t <- dupChan
  reader <- forkEff $ runM $ runNetworkHandle h $ runChan t $ readLoop name

  fix $ \loop -> do
    msg <- readMsg
    case msg of
      "quit" -> writeMsg "Bye!"
      _      -> do
        broadcast name $ name <> ": " <> msg
        putLog Info $ name <> " wrote: " <> msg
        loop

  putLog Info $ name <> " is leaving"
  killT reader
  close
  broadcast name $ "<-- " <> name <> " left."


readLoop :: (Members [Chan, Network, IO] es) => String -> Eff es ()
readLoop name = fix $ \loop -> do
    msg <- readChan name
    writeMsg msg
    loop
