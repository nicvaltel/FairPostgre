{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Domain.Messenger
  ( Messenger (..),
    LogData (..),
    ResultRow,
    Header,
    Headers,
    QueryAns (..),
    ResultValue (..),
    QueryStr,
  )
where

import Data.ByteString qualified as B
import Domain.Types

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

-- type QueryResult = (Headers, [ResultRow])

type Header = String

type Headers = [Header]

type QueryStr = B.ByteString

-- type MessageFinished = Bool

data LogData
  = LogResult QueryAns
  | LogSendMessage SendMessage
  | LogRecieve B.ByteString
  | LogConnectorAns ConnectorAns
  | LogError B.ByteString
  deriving (Show)

class Monad m => Messenger m where
  sendMessage :: DBConnection -> SendMessage -> m ()
  recieveByteString :: DBConnection -> m B.ByteString
  execQuery :: DBConnection -> QueryStr -> m ()

-- logger :: m ()
