{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( App (..),
    runApp,
    Messenger (..),
    Connector (..),
    DBConnectionInfo (..),
    SendMessage (..),
    QueryStr,
  )
where

import Adapter.PostgreSQL.Connector qualified as PG
import Adapter.PostgreSQL.Messenger qualified as PG
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Domain.Connector
import Domain.Messenger
import Domain.Types
import Control.Monad.Catch (MonadThrow , MonadCatch)

newtype App a = App {unApp :: ReaderT DBConnectionInfo IO a}
  deriving (Applicative, Functor, Monad, MonadReader DBConnectionInfo, MonadIO, MonadFail, MonadThrow , MonadCatch)

instance Messenger App where
  sendMessage = PG.sendMsg
  execQuery = PG.execQuery
  recieveByteString = PG.recieveByteString

instance Connector App where
  getDBConnectionInfo = PG.getDBConnectionInfo
  connect = PG.connect
  closeConnection = PG.closeConn

runApp :: DBConnectionInfo -> App a -> IO a
runApp conf (App app) = runReaderT app conf
