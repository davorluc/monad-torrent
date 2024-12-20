{-# LANGUAGE OverloadedStrings #-}

module Torrent
  ( getPieceHashes,
    makeQuery,
    addressToIPAndPort,
    parsePort,
    readTorrentFile,
    TorrentType (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), decode, object, withObject, (.:), (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (ord)
import Data.Map (Map, lookup)
import Data.Map.Internal ((!))
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as T
import Decoder (DecodedValue (..), calculateInfoHash, decodeBencodedValue, decodedToByteString, decodedToDictionary, generateURLEncodedInfoHash, getPiecesFromString, sortInfo)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import Prelude

data TorrentType = TorrentType
  { fileName :: ByteString,
    outputPath :: ByteString,
    infoHash :: ByteString,
    pieceHashes :: [ByteString],
    fileLength :: Int,
    pieceLength :: Int,
    trackerUrl :: ByteString,
    peers :: [(ByteString, ByteString)]
  }
  deriving (Show)

getPieceHashes :: ByteString -> [ByteString]
getPieceHashes pieces = if B.length pieces < 20 then [] else piece : getPieceHashes rest
  where
    (piece, rest) = B.splitAt 20 pieces

makeQuery :: Map ByteString DecodedValue -> ByteString -> String
makeQuery json url = do
  let info = decodedToDictionary (json ! B.pack "info")
  let len = B.pack $ show $ info ! B.pack "length"
  let torrentInfoHash = generateURLEncodedInfoHash $ sortInfo info
  B.unpack $ B.concat [url, B.pack "?info_hash=", torrentInfoHash, B.pack "&peer_id=12349679991234567890&port=6881&uploaded=0&downloaded=0&left=", len, B.pack "&compact=1"]

peersToAddressList :: ByteString -> [(ByteString, ByteString)]
peersToAddressList input = if B.length input < 6 then [] else addressToIPAndPort ip : peersToAddressList rest
  where
    (ip, rest) = B.splitAt 6 input

addressToIPAndPort :: ByteString -> (ByteString, ByteString)
addressToIPAndPort address = do
  (B.intercalate (B.pack ".") $ map (B.pack . show . ord) (B.unpack ip), parsePort port)
  where
    (ip, port) = B.splitAt 4 address

parsePort :: ByteString -> ByteString
parsePort port = B.pack $ show $ ord (B.head port) * 256 + ord (B.last port)

readDownloadDirectory :: IO ByteString
readDownloadDirectory = do
  settingsContent <- LB.readFile "settings.json"
  let maybeSettings = decode settingsContent :: Maybe (Map String String)
  let downloadDir = maybe "./downloads" (fromMaybe "./downloads" . Data.Map.lookup "downloadDirectory") maybeSettings
  pure $ B.pack downloadDir

instance ToJSON TorrentType where
  toJSON t =
    object
      [ "fileName" .= B.unpack (fileName t),
        "outputPath" .= B.unpack (outputPath t),
        "infoHash" .= B.unpack (infoHash t),
        "pieceHashes" .= Prelude.map B.unpack (pieceHashes t),
        "fileLength" .= fileLength t,
        "pieceLength" .= pieceLength t,
        "trackerUrl" .= B.unpack (trackerUrl t),
        "peers" .= Prelude.map (\(ip, port) -> (B.unpack ip, B.unpack port)) (peers t)
      ]

instance FromJSON TorrentType where
  parseJSON = withObject "TorrentType" $ \v -> do
    fileName' <- T.encodeUtf8 <$> v .: "fileName"
    outputPath' <- T.encodeUtf8 <$> v .: "outputPath"
    infoHash' <- T.encodeUtf8 <$> v .: "infoHash"
    pieceHashes' <- Prelude.map T.encodeUtf8 <$> v .: "pieceHashes"
    fileLength' <- v .: "fileLength"
    pieceLength' <- v .: "pieceLength"
    trackerUrl' <- T.encodeUtf8 <$> v .: "trackerUrl"
    peers' <- Prelude.map (\(ip, port) -> (T.encodeUtf8 ip, T.encodeUtf8 port)) <$> v .: "peers"
    pure $
      TorrentType
        { fileName = fileName',
          outputPath = outputPath',
          infoHash = infoHash',
          pieceHashes = pieceHashes',
          fileLength = fileLength',
          pieceLength = pieceLength',
          trackerUrl = trackerUrl',
          peers = peers'
        }

readTorrentFile :: ByteString -> IO TorrentType
readTorrentFile filePath = do
  fileContent <- LB.readFile $ B.unpack filePath
  downloadDir <- readDownloadDirectory
  let json = decodedToDictionary $ decodeBencodedValue (B.concat $ LB.toChunks fileContent)
  let info = decodedToDictionary (json ! "info")
  let torrentInfoHash = calculateInfoHash (sortInfo info) ""
  let torrentPieceLength = read (show $ info ! "piece length") :: Int
  let torrentFileLength = read (show $ info ! "length") :: Int
  let fileName = decodedToByteString $ info ! "name"
  let filePath = B.concat [downloadDir, fileName]
  let pieceHashesList = getPiecesFromString $ decodedToByteString (info ! "pieces")

  let trackerUrl = decodedToByteString (json ! "announce")

  let query = makeQuery json trackerUrl
  request <- parseRequest query
  response <- httpLBS request
  let peersBytes = decodedToDictionary (decodeBencodedValue $ LB.toStrict $ getResponseBody response) ! "peers"
  let peersList = peersToAddressList $ decodedToByteString peersBytes

  pure $
    TorrentType
      { peers = peersList,
        trackerUrl = trackerUrl,
        fileName = fileName,
        outputPath = filePath,
        infoHash = torrentInfoHash,
        pieceHashes = pieceHashesList,
        fileLength = torrentFileLength,
        pieceLength = torrentPieceLength
      }
