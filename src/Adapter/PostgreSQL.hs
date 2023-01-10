{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Adapter.PostgreSQL where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks, liftIO)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteArray.Encoding qualified as BA
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BLDR
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Internal (smallChunkSize)
import Data.Char (chr)
import Data.Has
import Data.Word (Word32)
import Domain.Messenger
import Adapter.ResponseParser ( selectResponseParser )
import Logger qualified as L
import Network.Socket (AddrInfo)
import Network.Socket qualified as Net
import Network.Socket.ByteString qualified as NetBS
import Utils
import Debug.Trace (traceShow)

type PostgreDB r m = (Has DBConnectionInfo r, MonadReader r m, MonadIO m)



getDBConnectionInfo :: PostgreDB r m => m DBConnectionInfo
getDBConnectionInfo = asks getter

connect :: PostgreDB r m => m DBConnection
connect = do
  (conf :: DBConnectionInfo) <- asks getter
  addr <- liftIO $ headAddrInfo <$> Net.getAddrInfo (Just addrInfo) (Just conf.hostName) (Just conf.port)
  socket <- liftIO $ Net.socket (Net.addrFamily addr) (Net.addrSocketType addr) (Net.addrProtocol addr)
  liftIO $ Net.connect socket (Net.addrAddress addr)
  let conn = DBSocket socket
  sendMsg conn conf.startupMsg
  answer <- parseAnswer <$> recieveByteString conn  
  logger (LogConnectorAns answer)
  res <- processAnswer answer conn
  case res of
    Left err -> error (show err)
    Right conn_ -> pure conn_
  -- pure conn
  where
    headAddrInfo :: [AddrInfo] -> AddrInfo
    headAddrInfo = \case
      [aInfo] -> aInfo
      [] -> error "Connection error: address info not received"
      _ -> error "Connection error: several info not received"

    addrInfo = Net.defaultHints{ Net.addrSocketType = Net.Stream }

-- | Closing connection
closeConn :: PostgreDB r m => DBConnection -> m ()
closeConn conn = sendMsg conn CloseConnection

parseAnswer :: BC.ByteString -> ConnectorAns
parseAnswer answer =
  case BC.uncons answer of
    Nothing -> ErrorResponse "Incorrect answer"
    Just (w, bs) -> parse' w bs
  where
    parse' :: Char -> BC.ByteString -> ConnectorAns
    parse' 'R' bs =
      let (w0, w1, bs') = split32'32 bs
       in parseAuthentication w0 w1 bs'
    parse' 'E' bs =
      let (_, bs') = BC.splitAt 5 bs
       in getHumanReadableMsg bs -- TODO or bs' ?
    parse' ch bs = UnimplementedConnectorAns $ [ch] <> show bs

    getHumanReadableMsg :: BC.ByteString -> ConnectorAns
    getHumanReadableMsg bs =
      case BC.uncons bs of
        Nothing -> ErrorResponse "Incorrect Human Readable Message"
        Just (w, bs') -> get' w bs'

    get' :: Char -> BC.ByteString -> ConnectorAns
    get' 'M' bs = ErrorResponse $ head $ BC.split (chr 0) bs
    get' _ bs = getHumanReadableMsg bs

    split32'32 :: BC.ByteString -> (Word32, Word32, BC.ByteString)
    split32'32 bs =
      let (w0, bs') = BC.splitAt 4 bs
          (w1, bs'') = BC.splitAt 4 bs'
       in (fromOctets $ B.unpack w0, fromOctets $ B.unpack w1, bs'')

parseAuthentication :: Word32 -> Word32 -> BC.ByteString -> ConnectorAns
parseAuthentication 8 0 bs = AuthenticationOk bs
parseAuthentication 8 2 _ = UnimplementedConnectorAns "AuthenticationKerberosV5"
parseAuthentication 8 3 _ = UnimplementedConnectorAns "AuthenticationCleartextPassword"
parseAuthentication 8 6 _ = UnimplementedConnectorAns "AuthenticationSCMCredential"
parseAuthentication 8 7 _ = UnimplementedConnectorAns "AuthenticationGSS"
parseAuthentication _ 8 bs = UnimplementedConnectorAns $ "AuthenticationGSSContinue" <> BC.unpack bs
parseAuthentication 8 9 _ = UnimplementedConnectorAns "AuthenticationSSPI"
parseAuthentication 8 10 bs = UnimplementedConnectorAns $ "AuthenticationSASL" <> BC.unpack bs
parseAuthentication 8 11 bs = UnimplementedConnectorAns $ "AuthenticationSASLContinue" <> BC.unpack bs
parseAuthentication 8 12 bs = UnimplementedConnectorAns $ "AuthenticationSASLFinal" <> BC.unpack bs
parseAuthentication 12 5 bs = AuthenticationMD5Password bs
parseAuthentication x y bs = UnimplementedConnectorAns $ "Undocumented response ... " <> show x <> " " <> show y <> " " <> BC.unpack bs -- threre are no such a responce according to official documentation

-- uses only for password authentication
processAnswer :: PostgreDB r m => ConnectorAns -> DBConnection -> m (Either ConnectorAns DBConnection)
processAnswer (AuthenticationOk _) conn = pure $ Right conn
processAnswer (AuthenticationMD5Password salt) conn = do
  (conf :: DBConnectionInfo) <- asks getter
  let saltedPass = md5 (md5 (BC.pack conf.password <> conf.username) <> salt) -- can be computed in SQL as concat('md5', md5(concat(md5(concat(password, username)), random-salt)))
  sendMsg conn (PasswordMessage $ "md5" <> saltedPass)
  answer <- parseAnswer <$> recieveByteString conn
  logger (LogConnectorAns answer)
  processAnswer answer conn -- now it should be AuthenticationOk
processAnswer ans _ = pure $ Left ans -- all other respons are Error

-- for password authentication
md5 :: B.ByteString -> B.ByteString
md5 bs = BA.convertToBase BA.Base16 (MD5.hash bs)

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
    len            = B.length bs
    (_, bs')       = B.splitAt (len - 6) bs
    (Just (ch, bs'')) = B.uncons bs'

bsNul :: B.ByteString -> B.ByteString
bsNul s = s <> nul -- TODO change to COnstant

nul :: B.ByteString -- TODO change to COnstant
nul = BL.toStrict . BLDR.toLazyByteString . BLDR.word8 $ 0 -- TODO simplify if

logger :: PostgreDB r m => LogData -> m ()
logger logData = pure ()

-- textLogData :: LogData -> Text
-- textLogData (LogResult (QueryResult qr)) = "< QueryResult: " (BC.pack $ show qr) (paramSaveResultTo params)
-- logData (LogResult (ErrorQueryResult bs)) = logWithPrefix "< ErrorQueryResult: " bs (paramSaveResultTo params)
-- logData (LogResult (UnimplementedQueryAns bs)) = logWithPrefix "< UnimplementedQueryAns: " bs (paramSaveResultTo params)
-- logData (LogSendMessage (StartupMessage msg)) = logWithPrefix "> Sended Startup: " (BC.pack $ show msg) (paramLoggerSender params)
-- logData (LogSendMessage (PasswordMessage pwd)) = logWithPrefix "> Sended Password: " pwd (paramLoggerSender params)
-- logData (LogSendMessage (Query q)) = logWithPrefix "> Sended Query: " q (paramLoggerSender params)
-- logData (LogSendMessage CloseConnection) = logWithPrefix "> CloseConnection: " "" (paramLoggerSender params)
-- logData (LogConnectorAns (AuthenticationOk bs)) = logWithPrefix "< AuthenticationOk: " bs (paramLoggerConnector params)
-- logData (LogConnectorAns (AuthenticationMD5Password bs)) = logWithPrefix "< AuthenticationMD5Password: " bs (paramLoggerConnector params)
-- logData (LogConnectorAns (ErrorResponse bs)) = logWithPrefix "< ErrorResponse: " bs (paramLoggerConnector params)
-- logData (LogConnectorAns (UnimplementedConnectorAns str)) = logWithPrefix "< AuthenticationMD5Password: " (BC.pack str) (paramLoggerConnector params)
-- logData (LogError bs) = logWithPrefix "Error: " bs PrintToConsole

-- |Send queries and recieve the result
execQuery :: PostgreDB r m => DBConnection -> QueryStr -> m ()
execQuery conn query  = do
  sendMsg conn (Query query)
  bs <- recieveLongByteString conn
  let result = selectResponseParser bs [] []
  logger (LogResult result)
