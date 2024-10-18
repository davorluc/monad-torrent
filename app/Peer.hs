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
import Data.Char (chr, ord)
import Data.Int (Int32)
import Data.Word (Word8)
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
getSocketHandle ip port infoHash = do
  addrInfo <- getAddrInfo Nothing (Just $ B.unpack ip) (Just $ B.unpack port)
  let serverAddr = head addrInfo
  peerSocket <- socket (addrFamily serverAddr) Stream defaultProtocol
  Network.Socket.connect peerSocket (addrAddress serverAddr)
  handle <- socketToHandle peerSocket ReadWriteMode
  B.hPut handle $ B.concat [B.toStrict $ LB.singleton (19 :: Word8), B.pack "BitTorrent protocol", B.concat $ replicate 8 (B.toStrict $ LB.singleton (0 :: Word8)), infoHash, B.pack "12345678901234567890"]
  hFlush handle
  response <- B.hGet handle 68
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
      if hexByteStringToInt (B.singleton $ B.head message) == x then return () else waitForXDiscardMessage handle x

waitForX :: Handle -> Int -> IO Int
waitForX handle x = do
  prefix <- B.hGet handle 4
  let len = hexByteStringToInt prefix
  if len == 0
    then waitForX handle x
    else do
      message <- B.hGet handle 1
      if hexByteStringToInt (B.singleton $ B.head message) == x then return len else waitForX handle x

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
        getRequestField $ fromIntegral pieceIndex,
        getRequestField $ fromIntegral blockIndex,
        getRequestField $ fromIntegral blockLength
      ]
  hFlush handle

intToHexByteString :: Int32 -> ByteString
intToHexByteString n = B.toStrict $ toLazyByteString $ int32BE n

padWithZeros :: ByteString -> ByteString
padWithZeros bs
  | B.length bs >= 4 = bs -- If length is already >= 4, return the ByteString as is
  | otherwise = B.replicate (4 - B.length bs) (B.head $ B.toStrict $ LB.singleton (0 :: Word8)) `B.append` bs

getRequestField :: Int32 -> ByteString
getRequestField n = intToHexByteString n

makeBigEndian :: ByteString -> ByteString
makeBigEndian bs = B.concat [last, first]
  where
    (first, last) = B.splitAt 2 bs

receiveBlock :: Handle -> Int -> Int -> Int -> String -> IO ()
receiveBlock handle pieceIndex blockOffset blockLength outputFile = do
  print $ "Requesting block " <> show (blockOffset) <> " of piece " <> show pieceIndex <> " with length " <> show blockLength
  requestBlock handle pieceIndex blockOffset blockLength

  print "waiting for 7"
  _ <- waitForX handle 7
  print "received 7"
  consume handle 4 -- pieceIndexResponse
  consume handle 4 -- blockBeginResponse
  block <- B.hGet handle blockLength

  LB.appendFile outputFile (fromStrict block)

consume :: Handle -> Int -> IO ()
consume handle x = do
  _ <- B.hGet handle x
  return ()

getPiece :: Handle -> Int -> Int -> String -> Int -> IO ()
getPiece handle pieceIndex pieceLength outputPath fileLength = do
  let blockLength = 2 ^ 14
  let lastBlockLength = if (pieceIndex + 1) * pieceLength > fileLength then fileLength `mod` blockLength else 0
  let pieceLengthCalculated = min (pieceLength * (pieceIndex + 1)) fileLength - (pieceIndex * pieceLength)
  let numBlocks = pieceLengthCalculated `div` blockLength
  print $ "numBlocks: " <> show numBlocks
  print $ "lastBlockLength: " <> show lastBlockLength
  -- calculate block indices to be used in the mapM function later
  let blockIndices = [0 .. (numBlocks - 1)]

  print $ "blockIndices: " <> show blockIndices

  let blockLengths = replicate numBlocks blockLength

  -- request all blocks with normal length
  mapM_ (\i -> receiveBlock handle pieceIndex (blockLength * i) (blockLengths !! i) outputPath) blockIndices

  -- request final block if length is unusual
  -- TODO: validate hash of downloaded piece
  when (lastBlockLength > 0) $ receiveBlock handle pieceIndex (blockLength * numBlocks) lastBlockLength outputPath
