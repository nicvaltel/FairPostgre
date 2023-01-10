{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger where

import Control.Exception (SomeException, catch)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)

data LogParams = LogParams
  { paramSaveResultTo :: LogTo,
    paramLoggerConnector :: LogTo,
    paramLoggerSender :: LogTo,
    paramLoggerReciever :: LogTo
  }
  deriving (Show)

data LogTo
  = PrintToConsole
  | SaveToFile FilePath
  | PrintAndSaveToFile FilePath
  | TurnOff
  deriving (Show)

-- | Print data to display and save it to file according to WorkingParams
logger :: Text -> LogTo -> IO ()
logger txt logTo =
  catch
    (log txt logTo)
    (\e -> putStrLn "OMG, even the logger doesn't work!" >> print (e :: SomeException) >> print txt)
  where
    log :: Text -> LogTo -> IO ()
    log txt logTo = do
      time <- Text.pack . show <$> getCurrentTime
      let timedTxt = time <> " " <> txt
      case logTo of
        PrintToConsole -> TIO.putStrLn timedTxt
        SaveToFile f -> TIO.appendFile f timedTxt
        PrintAndSaveToFile f -> TIO.putStrLn timedTxt >> TIO.appendFile f timedTxt
        TurnOff -> pure ()
