{-# LANGUAGE GADTs, FlexibleContexts, TemplateHaskell, TypeOperators, DataKinds #-}

module Effects.Logging
( Logger
, LogLevel(..)
, putLog
, runLogger
, runFileLogger
, runSilentLogger
) where

import Data.Time
import Control.Monad.Freer
import Control.Monad.Freer.TH

data LogLevel = Info
              | Warning
              | Fatal
              deriving Show

data Logger r where
  PutLog  :: LogLevel -> String -> Logger ()
makeEffect ''Logger

mkLog :: LogLevel -> String -> IO String
mkLog level msg = do
  now <- getCurrentTime
  let timeLog = formatTime defaultTimeLocale "[%d/%m/%Y][%H:%M:%S]" now
  return $ "[" <> show level <> "]" <> timeLog <> "\t" <> msg

runLogger :: LastMember IO es => Eff (Logger : es) a -> Eff es a
runLogger = interpretM go
  where go :: Logger r -> IO r
        go (PutLog level msg) = mkLog level msg >>= putStrLn

runFileLogger :: LastMember IO es => FilePath -> Eff (Logger : es) a -> Eff es a
runFileLogger fp = interpretM go
  where go :: Logger r -> IO r
        go (PutLog level msg) = mkLog level msg >>= \s -> appendFile fp (s <> "\n")


runSilentLogger :: Eff (Logger : es) a -> Eff es a
runSilentLogger = interpret go
  where go :: Logger r -> Eff es r
        go (PutLog _ _) = return ()
