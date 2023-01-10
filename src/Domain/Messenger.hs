{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Domain.Messenger where

import Data.ByteString qualified as B
import Network.Socket (AddrInfo, HostName)
import Network.Socket qualified as Net
import Logger qualified as L


data SendMessage
  = StartupMessage [(B.ByteString, B.ByteString)]
  | PasswordMessage B.ByteString
  | Query B.ByteString
  | CloseConnection
  deriving (Show)

type Port = String

data DBConnectionInfo = DBConnectionInfo
  { 
    hostName :: HostName,
    port :: Port,
    dbName :: B.ByteString,
    startupMsg :: SendMessage,
    logParams :: L.LogParams,
    username :: B.ByteString,
    password :: String -- TODO if password is not saved, ask for it
  }

newtype DBConnection = DBSocket {connHandle :: Net.Socket}



data ConnectorAns
  = AuthenticationOk B.ByteString
  | AuthenticationMD5Password B.ByteString
  | ErrorResponse B.ByteString
  | UnimplementedConnectorAns String
  deriving (Show)

data QueryAns
  = QueryResult (Headers, [ResultRow])
  | ErrorQueryResult B.ByteString
  | UnimplementedQueryAns B.ByteString
  deriving (Show)

data ResultValue
  = NullVal
  | BinaryVal B.ByteString
  deriving (Show)

type ResultRow = [ResultValue]

type QueryResult = (Headers, [ResultRow])

type Header = String

type Headers = [Header]

type QueryStr = B.ByteString

type MessageFinished = Bool


data LogData
  = LogResult QueryAns
  | LogSendMessage SendMessage
  | LogRecieve B.ByteString
  | LogConnectorAns ConnectorAns
  | LogError B.ByteString
  deriving (Show)



class Monad m => Messenger m where
  getDBConnectionInfo :: m DBConnectionInfo
  connect :: m DBConnection
  closeConnection :: DBConnection -> m ()
  sendMessage :: DBConnection -> SendMessage -> m ()
  execQuery :: DBConnection -> QueryStr -> m ()
  -- logger :: m ()


  

-- -- |Main routine
-- routine :: ReaderT AllConfig IO ()
-- routine = do
--   db         <- asks confDb
--   startupMsg <- asks confStartupMsg
--   eitherConn       <- dbConnect
--   params <- asks confParams
--   conf <- ask

--   query <- asks confQuery
--   case eitherConn of 
--               Left err -> liftIO $ logger (LogError $ BC.pack $ show err) params
--               Right conn -> do
--                 local (\c -> c {confDBConnection = conn}) execQuery
--                 liftIO $ closeConn conn params
  