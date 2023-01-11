{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Domain.Types where

import Data.ByteString qualified as B
import Logger qualified as L
import Network.Socket (HostName)
import Network.Socket qualified as Net

type Port = String

data DBConnectionInfo = DBConnectionInfo
  { hostName :: HostName,
    port :: Port,
    dbName :: B.ByteString,
    startupMsg :: SendMessage,
    logParams :: L.LogParams,
    username :: B.ByteString,
    password :: String -- TODO if password is not saved, ask for it
  }

newtype DBConnection = DBSocket {connHandle :: Net.Socket}

data SendMessage
  = StartupMessage [(B.ByteString, B.ByteString)]
  | PasswordMessage B.ByteString
  | Query B.ByteString
  | CloseConnection
  deriving (Show)

data ConnectorAns
  = AuthenticationOk B.ByteString
  | AuthenticationMD5Password B.ByteString
  | ErrorResponse B.ByteString
  | UnimplementedConnectorAns String
  deriving (Show)
