{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Adapter.PostgreSQL qualified as PG
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Domain.Messenger

newtype App a = App {unApp :: ReaderT DBConnectionInfo IO a}
  deriving (Applicative, Functor, Monad, MonadReader DBConnectionInfo, MonadIO, MonadFail)

instance Messenger App where
  getDBConnectionInfo = PG.getDBConnectionInfo
  connect = PG.connect
  closeConnection = PG.closeConn
  sendMessage = PG.sendMsg
  execQuery = PG.execQuery


runApp :: DBConnectionInfo -> App a -> IO a
runApp conf (App app) = runReaderT app conf



  

