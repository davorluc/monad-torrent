{-# LANGUAGE OverloadedStrings #-}

module Torrent
  ( -- generateURLEncodedInfoHash,
    -- calculateInfoHash,
    -- calculateHash,
    -- calculateHexHash,
    getPieceHashes,
    makeQuery,
    addressToIPAndPort,
    parsePort,
    readTorrentFile,
    TorrentType (..),
  )
where

import Data.Aeson (decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (ord)
import Data.Map (Map, lookup)
import Data.Map.Internal ((!))
import Data.Maybe (fromMaybe)
import Decoder (DecodedValue (..), calculateInfoHash, decodeBencodedValue, decodedToByteString, decodedToDictionary, generateURLEncodedInfoHash, getPieces, sortInfo)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.IO (readFile)
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
makeQuery json trackerUrl = do
  let info = decodedToDictionary (json ! B.pack "info")
  let len = B.pack $ show $ info ! B.pack "length"
  let torrentInfoHash = generateURLEncodedInfoHash $ sortInfo info
  B.unpack $ B.concat [trackerUrl, B.pack "?info_hash=", torrentInfoHash, B.pack "&peer_id=12349679991234567890&port=6881&uploaded=0&downloaded=0&left=", len, B.pack "&compact=1"]

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
  let pieceHashesList = getPieces $ decodedToByteString (info ! "pieces")

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
