{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Adapter.PostgreSQL.Connector (getDBConnectionInfo, connect, closeConn) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks, liftIO)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteArray.Encoding qualified as BA
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Char (chr)
import Data.Has
import Data.Word (Word32)
import Domain.Messenger
import Domain.Types
import Network.Socket (AddrInfo)
import Network.Socket qualified as Net
import Utils

type PostgreDB r m = (Has DBConnectionInfo r, MonadReader r m, MonadIO m, Messenger m)

getDBConnectionInfo :: PostgreDB r m => m DBConnectionInfo
getDBConnectionInfo = asks getter

connect :: PostgreDB r m => m DBConnection
connect = do
  (conf :: DBConnectionInfo) <- asks getter
  addr <- liftIO $ headAddrInfo <$> Net.getAddrInfo (Just addrInfo) (Just conf.hostName) (Just conf.port)
  socket <- liftIO $ Net.socket (Net.addrFamily addr) (Net.addrSocketType addr) (Net.addrProtocol addr)
  liftIO $ Net.connect socket (Net.addrAddress addr)
  let conn = DBSocket socket
  sendMessage conn conf.startupMsg
  answer <- parseAnswer <$> recieveByteString conn
  res <- processAnswer answer conn
  case res of  -- TODO maybe use catch?
    Left err -> error (show err)
    Right conn_ -> pure conn_
  where
    headAddrInfo :: [AddrInfo] -> AddrInfo
    headAddrInfo = \case
      [aInfo] -> aInfo
      [] -> error "Connection error: address info not received"
      _ -> error "Connection error: several info not received"

    addrInfo = Net.defaultHints {Net.addrSocketType = Net.Stream}

-- | Closing connection
closeConn :: PostgreDB r m => DBConnection -> m ()
closeConn conn = sendMessage conn CloseConnection

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
  sendMessage conn (PasswordMessage $ "md5" <> saltedPass)
  answer <- parseAnswer <$> recieveByteString conn
  processAnswer answer conn -- now it should be AuthenticationOk
processAnswer ans _ = pure $ Left ans -- all other respons are Error

-- for password authentication
md5 :: B.ByteString -> B.ByteString
md5 bs = BA.convertToBase BA.Base16 (MD5.hash bs)
