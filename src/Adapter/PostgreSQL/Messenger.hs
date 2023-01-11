{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Adapter.PostgreSQL.Messenger (sendMsg, recieveByteString, execQuery) where

import Adapter.PostgreSQL.ResponseParser (selectResponseParser)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, liftIO)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BLDR
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Internal (smallChunkSize)
import Data.Has
import Data.Word (Word32)
import Domain.Messenger
    ( LogData(LogResult, LogSendMessage), QueryStr )
import Domain.Types
import Network.Socket.ByteString qualified as NetBS
import Utils

type PostgreDB r m = (Has DBConnectionInfo r, MonadReader r m, MonadIO m)

-- | Sends a message to connected DB. Official guide https://www.postgresql.org/docs/13/protocol-message-formats.html
sendMsg :: PostgreDB r m => DBConnection -> SendMessage -> m ()
sendMsg conn msg = do
  let (ch, bs) = message msg
  logger (LogSendMessage msg)
  case ch of
    Just c -> sendByteString conn $ BC.singleton c <> B.pack (octets (fromIntegral $ 4 + B.length bs :: Word32)) <> bs
    Nothing -> sendByteString conn $ B.pack (octets (fromIntegral $ 4 + B.length bs :: Word32)) <> bs

sendByteString :: PostgreDB r m => DBConnection -> B.ByteString -> m ()
sendByteString (DBSocket s) = liftIO . NetBS.sendAll s

message :: SendMessage -> (Maybe Char, B.ByteString)
message (Query bs) = (Just 'Q', bs <> nul)
message CloseConnection = (Just 'C', bsNul "P")
message (PasswordMessage s) = (Just 'p', bsNul s)
message (StartupMessage params_) = (Nothing, paramsBS)
  where
    paramsBS :: B.ByteString
    paramsBS =
      bs30000
        <> mconcat [bsNul k <> bsNul v | (k, v) <- params_]
        <> nul

    bs30000 :: B.ByteString
    bs30000 = BL.toStrict . BLDR.toLazyByteString . BLDR.word32BE $ 0x30000 -- TODO simplify if

-- | when size of message <= smallChunkSize
recieveByteString :: PostgreDB r m => DBConnection -> m B.ByteString
recieveByteString conn = readFromSocket conn smallChunkSize

readFromSocket :: PostgreDB r m => DBConnection -> Int -> m BC.ByteString
readFromSocket (DBSocket s) = liftIO . NetBS.recv s

-- | when size of message > smallChunkSize
recieveLongByteString :: PostgreDB r m => DBConnection -> m B.ByteString
recieveLongByteString conn = do
  bs <- readFromSocket conn smallChunkSize
  if checkForEndMsg bs
    then pure bs
    else do
      bsEnd <- recieveLongByteString conn
      pure $ bs `B.append` bsEnd

checkForEndMsg :: B.ByteString -> Bool
checkForEndMsg bs = w2c ch == 'Z'
  where
    len = B.length bs
    (_, bs') = B.splitAt (len - 6) bs
    (Just (ch, bs'')) = B.uncons bs'

bsNul :: B.ByteString -> B.ByteString
bsNul s = s <> nul -- TODO change to COnstant

nul :: B.ByteString -- TODO change to COnstant
nul = BL.toStrict . BLDR.toLazyByteString . BLDR.word8 $ 0 -- TODO simplify if

logger :: PostgreDB r m => LogData -> m ()
logger logData = pure ()

-- | Send queries and recieve the result
execQuery :: PostgreDB r m => DBConnection -> QueryStr -> m ()
execQuery conn query = do
  sendMsg conn (Query query)
  bs <- recieveLongByteString conn
  let result = selectResponseParser bs [] []
  logger (LogResult result)
