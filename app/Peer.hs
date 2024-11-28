module Peer
  ( getSocketHandle,
    waitForBitfield,
    waitForUnchoke,
    sendInterested,
    getPiece,
  )
where

import Control.Monad (when)
import Data.Bits (shiftL)
import Data.ByteString (ByteString, foldl', fromStrict, hPut, pack, split)
import Data.ByteString.Builder (int32BE, toLazyByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Base16 as B16
import Data.Char (chr, ord)
import Data.Int (Int32)
import Data.Map ((!))
import Data.Word (Word8)
import Decoder
import GHC.IO.Handle
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Network.HTTP.Conduit (parseRequest)
import Network.HTTP.Simple (getResponseBody, httpLBS)
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily),
    SocketType (Stream),
    connect,
    defaultProtocol,
    getAddrInfo,
    socket,
    socketToHandle,
  )
import Torrent ( Torrent ) 

getSocketHandle :: ByteString -> ByteString -> ByteString -> IO Handle
getSocketHandle ip port infoHash = do
  addrInfo <- getAddrInfo Nothing (Just $ B.unpack ip) (Just $ B.unpack port)
  let serverAddr = head addrInfo
  peerSocket <- socket (addrFamily serverAddr) Stream defaultProtocol
  Network.Socket.connect peerSocket (addrAddress serverAddr)
  handle <- socketToHandle peerSocket ReadWriteMode
  B.hPut handle $ B.concat [B.toStrict $ LB.singleton (19 :: Word8), B.pack "BitTorrent protocol", B.concat $ replicate 8 (B.toStrict $ LB.singleton (0 :: Word8)), infoHash, B.pack "12345678901234567890"]
  hFlush handle
  return handle

waitForBitfield :: Handle -> IO ()
waitForBitfield handle = waitForXDiscardMessage handle 5

waitForUnchoke :: Handle -> IO ()
waitForUnchoke handle = waitForXDiscardMessage handle 1

waitForXDiscardMessage :: Handle -> Int -> IO ()
waitForXDiscardMessage handle x = do
  prefix <- B.hGet handle 4
  let length = hexByteStringToInt prefix
  if length == 0
    then waitForXDiscardMessage handle x
    else do
      message <- B.hGet handle length
      if hexByteStringToInt (B.singleton $ B.head message) == x
        then return ()
        else waitForXDiscardMessage handle x

waitForX :: Handle -> Int -> IO Int
waitForX handle x = do
  prefix <- B.hGet handle 4
  let length = hexByteStringToInt prefix
  if length == 0
    then waitForX handle x
    else do
      message <- B.hGet handle 1
      if hexByteStringToInt (B.singleton $ B.head message) == x
        then return length
        else waitForX handle x

sendInterested :: Handle -> IO ()
sendInterested handle = do
  -- TODO use Megaparsec to construct message
  B.hPut handle $ B.concat [padWithZeros $ B.toStrict $ LB.singleton (1 :: Word8), B.toStrict $ LB.singleton (2 :: Word8)]
  hFlush handle

hexByteStringToInt :: ByteString -> Int
hexByteStringToInt = foldl' step 0
  where
    step acc c =
      let digit = ord (chr (fromIntegral c))
       in acc `shiftL` 4 + digit

requestBlock :: Handle -> Int -> Int -> Int -> IO ()
requestBlock handle pieceIndex blockIndex blockLength = do
  -- TODO use Megaparsec to construct message
  B.hPut handle $
    B.concat
      [ padWithZeros $ B.toStrict $ LB.singleton (13 :: Word8),
        B.toStrict $ LB.singleton (6 :: Word8),
        intToHexByteString $ fromIntegral pieceIndex,
        intToHexByteString $ fromIntegral blockIndex,
        intToHexByteString $ fromIntegral blockLength
      ]
  hFlush handle

receiveBlock :: Handle -> Int -> Int -> Int -> IO ByteString
receiveBlock handle pieceIndex blockOffset blockLength = do
  -- print $ "Requesting block " <> show (blockOffset) <> " of piece " <> show pieceIndex <> " with length " <> show blockLength
  requestBlock handle pieceIndex blockOffset blockLength

  messageLength <- waitForX handle 7
  pieceIndexResponse <- B.hGet handle 4
  blockBeginResponse <- B.hGet handle 4
  B.hGet handle blockLength

getPiece :: Handle -> Int -> Int -> Int -> IO ByteString
getPiece handle pieceIndex pieceLength fileLength = do
  let blockLength = 2 ^ 14
  let lastBlockLength = if (pieceIndex + 1) * pieceLength > fileLength then fileLength `mod` blockLength else 0
  let pieceLengthCalculated = min (pieceLength * (pieceIndex + 1)) fileLength - (pieceIndex * pieceLength)
  let numBlocks = pieceLengthCalculated `div` blockLength

  let blockIndices = [0 .. (numBlocks - 1)]

  let blockLengths = replicate numBlocks blockLength
  -- get all blocks with normal length
  blocks <- mapM (\i -> (receiveBlock handle pieceIndex (blockLength * i) (blockLengths !! i))) blockIndices

  -- get final block if length is unusual
  if lastBlockLength > 0
    then do
      lastBlock <- receiveBlock handle pieceIndex (blockLength * length blockIndices) lastBlockLength
      return $ B.concat (blocks ++ [lastBlock])
    else return $ B.concat blocks

getReadyHandle :: (ByteString, ByteString) -> ByteString -> IO Handle
getReadyHandle peerAddress infoHash = do
  handle <- getSocketHandle (fst peerAddress) (snd peerAddress) infoHash
  peerHandshakeResponse <- B.hGet handle 68 -- peer handshake
  waitForBitfield handle
  sendInterested handle
  waitForUnchoke handle
  return handle

downloadFile :: Torrent -> IO ()
downloadFile torrent = do
  --TODO access torrent fields
  let peerAdress = head $ (peers torrent)
  handle <- getReadyHandle peerAddress (infoHash torrent)

  let pieces = map (\i -> getPiece handle i (pieceLength torrent) (fileLength torrent)) pieceIndices
  piecesContent <- sequence pieces

  let piecesHashes = map calculateHash piecesContent
  let piecesHashesHex = map B16.encode piecesHashes
  let piecesHashesExpected = (pieceHashes torrent)
  let piecesHashesExpectedHex = map B16.encode piecesHashesExpected
  if all (== True) $ zipWith (==) piecesHashesHex piecesHashesExpectedHex
    then putStrLn "All pieces hashes match!"
    else putStrLn "Some pieces hashes do not match!"

  LB.writeFile outputPath (LB.fromStrict $ B.concat piecesContent)

  putStrLn "All pieces received!"

  hClose handle