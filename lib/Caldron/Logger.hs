{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Caldron.Logger
  ( writeLog
  ) where

import Protolude

import Control.Monad.Logger (LogLevel(..))
import qualified Data.Text.IO as T
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)

-- Simple console logger
writeLog :: LogLevel -> Text -> IO ()
writeLog level msg = do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
      levelStr = case level of
        LevelDebug -> "[DEBUG]"
        LevelInfo -> "[INFO] "
        LevelWarn -> "[WARN] "
        LevelError -> "[ERROR]"
        LevelOther other -> "[" <> other <> "]"
      logLine = toS timeStr <> " " <> levelStr <> " " <> msg
  T.putStrLn logLine