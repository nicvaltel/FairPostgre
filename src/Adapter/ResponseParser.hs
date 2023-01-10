{-# LANGUAGE OverloadedStrings #-}
module Adapter.ResponseParser (selectResponseParser) where


import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal (w2c)


import Domain.Messenger
import Utils ( fromOctets, fromOctets16 )
import Data.Word ( Word8 )



-- |Parse answer to query All according to official guide https://www.postgresql.org/docs/13/protocol-message-formats.html
selectResponseParser :: B.ByteString -> Headers -> [ResultRow] -> QueryAns
selectResponseParser input hs res =
    case B.uncons input of
      Nothing       -> ErrorQueryResult "Incorrect query result"
      Just (w, bs)  -> parse (w2c w) bs

    where
      parse :: Char -> B.ByteString -> QueryAns
      parse 'T' bs = let (w4, bs') = B.splitAt 4 bs
                         len = fromIntegral $ fromOctets $ B.unpack w4 :: Int
                         (rowDescription, bs'') = B.splitAt (len -4) bs'
                         eitherHeaders = parseRowDescription rowDescription
                         in case eitherHeaders of
                              Right headers -> selectResponseParser bs'' headers res
                              Left bs -> ErrorQueryResult bs
      parse 'D' bs = let (w4, bs') = B.splitAt 4 bs
                         len = fromIntegral $ fromOctets $ B.unpack w4 :: Int
                         (w2, bs'') = B.splitAt 2 bs'
                         n = fromIntegral $ fromOctets16 $ B.unpack w2 :: Int
                         (dataRow, bs''') = B.splitAt (len - 6) bs''
                     in selectResponseParser bs''' hs (parseRow n [] dataRow:res)
      parse 'C' bs = QueryResult (hs, reverse res)
      parse c bs = ErrorQueryResult $ "Unexpected query result " <> BC.pack (c:" ") <> bs


      parseRowDescription :: B.ByteString -> Either B.ByteString Headers
      parseRowDescription bs = let (w2, bs') = B.splitAt 2 bs
                                   n = fromIntegral $ fromOctets16 $ B.unpack w2 :: Int
                               in parseHeader n bs' []


      parseHeader :: Int -> B.ByteString -> Headers -> Either B.ByteString Headers
      parseHeader 0 bs hs = Right $ reverse hs
      parseHeader n bs hs =
          case B.uncons bs of
              Nothing       -> Left "Incorrect query result headers"
              Just (w, bs') -> case parseName w bs' [] of
                                   Right (name, bs'') -> parseHeader (n-1) bs'' (name:hs)
                                   Left bs -> Left bs

      parseName :: Word8 -> B.ByteString -> Header -> Either B.ByteString (Header, B.ByteString)
      parseName 0 bs name = let (_, bs') = B.splitAt 18 bs in Right (reverse name, bs') -- delete last 18 Bytes with metadata, without field name
      parseName w bs name =
          case B.uncons bs of
              Nothing      -> Left "Incorrect query result Name"
              Just (w', bs') -> parseName w' bs' (w2c w:name)

      parseRow :: Int -> ResultRow -> B.ByteString -> ResultRow
      parseRow 0 rr _ = reverse rr
      parseRow n rr bs =
          let (w4, bs') = B.splitAt 4 bs
              len = fromIntegral $ fromOctets $ B.unpack w4 :: Int
              (cellData, bs'') = B.splitAt len bs'
          in parseRow (n-1) (resVal cellData : rr) bs''


      resVal :: B.ByteString -> ResultValue
      resVal bs
        | B.null bs = NullVal
        | otherwise  = BinaryVal bs
