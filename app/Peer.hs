module Peer
  ( getSocketHandle,
    waitForBitfield,
    waitForUnchoke,
    sendInterested,
    getPiece,
    downloadFile,
  )
where

import Data.Bits (shiftL)
import Data.ByteString (ByteString, foldl')
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (chr, ord)
import Data.Word (Word8)
import Decoder (calculateHash, intToHexByteString, padWithZeros)
import GHC.IO.Handle
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily),
    SocketType (Stream),
    connect,
    defaultProtocol,
    getAddrInfo,
    socket,
    socketToHandle,
  )
import Torrent

getSocketHandle :: ByteString -> ByteString -> ByteString -> IO Handle
getSocketHandle ip port fileInfoHash = do
  addrInfo <- getAddrInfo Nothing (Just $ B.unpack ip) (Just $ B.unpack port)
  let serverAddr = head addrInfo
  peerSocket <- socket (addrFamily serverAddr) Stream defaultProtocol
  Network.Socket.connect peerSocket (addrAddress serverAddr)
  handle <- socketToHandle peerSocket ReadWriteMode
  B.hPut handle $ B.concat [B.toStrict $ LB.singleton (19 :: Word8), B.pack "BitTorrent protocol", B.concat $ replicate 8 (B.toStrict $ LB.singleton (0 :: Word8)), fileInfoHash, B.pack "12345678901234567890"]
  hFlush handle
  return handle

waitForBitfield :: Handle -> IO ()
waitForBitfield handle = waitForXDiscardMessage handle 5

waitForUnchoke :: Handle -> IO ()
waitForUnchoke handle = waitForXDiscardMessage handle 1

waitForXDiscardMessage :: Handle -> Int -> IO ()
waitForXDiscardMessage handle x = do
  prefix <- B.hGet handle 4
  let len = hexByteStringToInt prefix
  if len == 0
    then waitForXDiscardMessage handle x
    else do
      message <- B.hGet handle len
      if hexByteStringToInt (B.singleton $ B.head message) == x
        then return ()
        else waitForXDiscardMessage handle x

waitForX :: Handle -> Int -> IO Int
waitForX handle x = do
  prefix <- B.hGet handle 4
  let len = hexByteStringToInt prefix
  if len == 0
    then waitForX handle x
    else do
      message <- B.hGet handle 1
      if hexByteStringToInt (B.singleton $ B.head message) == x
        then return len
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

  _ <- waitForX handle 7 -- message length
  _ <- B.hGet handle 4 -- piece Index response
  _ <- B.hGet handle 4 -- block begin response
  B.hGet handle blockLength

getPiece :: Handle -> Int -> Int -> Int -> IO ByteString
getPiece handle pieceIndex pieceLen fileLen = do
  let blockLength = 16384 -- 2^14
  let lastBlockLength = if (pieceIndex + 1) * pieceLen > fileLen then fileLen `mod` blockLength else 0
  let pieceLenCalculated = min (pieceLen * (pieceIndex + 1)) fileLen - (pieceIndex * pieceLen)
  let numBlocks = pieceLenCalculated `div` blockLength

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
getReadyHandle peerAddress fileInfoHash = do
  handle <- getSocketHandle (fst peerAddress) (snd peerAddress) fileInfoHash
  _ <- B.hGet handle 68 -- peer handshake response
  waitForBitfield handle
  sendInterested handle
  waitForUnchoke handle
  return handle

downloadFile :: TorrentType -> IO Bool
downloadFile torrent = do
  let peerAddress = head $ peers torrent

  handle <- getReadyHandle peerAddress (infoHash torrent)
  let numPieces = fileLength torrent `div` pieceLength torrent
  let pieceIndices = [0 .. numPieces]
  let pieces = map (\i -> getPiece handle i (pieceLength torrent) (fileLength torrent)) pieceIndices
  piecesContent <- sequence pieces

  let piecesHashes = map calculateHash piecesContent
  let piecesHashesHex = map B16.encode piecesHashes
  let piecesHashesExpected = (pieceHashes torrent)
  let piecesHashesExpectedHex = map B16.encode piecesHashesExpected

  -- do check for each piece hash
  -- apply do while loop, if there is a mismatch, request the piece from a different peer
  -- write successful pieces to file with offset = pieceIndex * pieceLength
  LB.writeFile (B.unpack (outputPath torrent)) (LB.fromStrict $ B.concat piecesContent)

  -- putStrLn "All pieces received!"

  hClose handle

  if and $ zipWith (==) piecesHashesHex piecesHashesExpectedHex
    then return True
    else return False
