module Peer
  ( getSocketHandle,
    waitForBitfield,
    waitForUnchoke,
    sendInterested,
    getPiece,
    downloadFile,
  )
where

import Control.Concurrent.Async (async, wait)
import Control.Exception (IOException)
import Control.Exception.Base (try)
import Control.Monad
import Data.Bits (shiftL)
import Data.ByteString (ByteString, foldl', hPut)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (chr, ord)
import Data.Word (Word8)
import Decoder (intToHexByteString, padWithZeros)
import GHC.Conc.Sync
import GHC.IO.Handle
import GHC.IO.IOMode
import GHC.IO.StdHandles
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily),
    SocketType (Stream),
    connect,
    defaultProtocol,
    getAddrInfo,
    socket,
    socketToHandle,
  )
import System.Directory
import Torrent

type Piece = Int

type PieceQueue = TVar [Piece]

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
  requestBlock handle pieceIndex blockOffset blockLength

  _ <- waitForX handle 7 -- message length
  _ <- B.hGet handle 4 -- piece Index response
  _ <- B.hGet handle 4 -- block begin response
  B.hGet handle blockLength

-- Attempt to open a binary file and handle the error if it is locked
openFileMaybe :: FilePath -> IOMode -> IO (Maybe Handle)
openFileMaybe path mode = do
  result <- try (openBinaryFile path mode) :: IO (Either IOException Handle)
  case result of
    Left _ -> return Nothing -- If an IOException occurs, return Nothing
    Right h -> return (Just h) -- Otherwise, return Just Handle

writeToFileAtOffset :: FilePath -> Integer -> ByteString -> IO (Maybe ())
writeToFileAtOffset path offset content = do
  -- Check if the file exists
  handle <- openFileMaybe path ReadWriteMode -- Open in read-write mode if it exists
  case handle of
    Nothing -> do
      return Nothing
    Just h -> do
      -- Ensure the file is at least `offset` bytes long
      currentSize <- hFileSize h
      when (currentSize < offset) $ hSetFileSize h offset -- Do nothing if the size is sufficient
      hSeek h AbsoluteSeek offset
      hPut h content
      hClose h
      return (Just ())

getPiece :: ByteString -> Handle -> Int -> Int -> Int -> IO (Maybe ())
getPiece outputPath handle pieceIndex pieceLength fileLength = do
  -- putStrLn $ "Getting piece " <> show pieceIndex
  let blockLength = 2 ^ 14
  let lastBlockLength = if (pieceIndex + 1) * pieceLength > fileLength then fileLength `mod` blockLength else 0
  let pieceLengthCalculated = min (pieceLength * (pieceIndex + 1)) fileLength - (pieceIndex * pieceLength)
  let numBlocks = pieceLengthCalculated `div` blockLength

  let blockIndices = [0 .. (numBlocks - 1)]

  let blockLengths = replicate numBlocks blockLength
  -- get all blocks with normal length
  blocks <- mapM (\i -> receiveBlock handle pieceIndex (blockLength * i) (blockLengths !! i)) blockIndices

  -- get final block if length is unusual
  piece <- do
    if lastBlockLength > 0
      then do
        lastBlock <- receiveBlock handle pieceIndex (blockLength * length blockIndices) lastBlockLength
        pure $ B.concat (blocks ++ [lastBlock])
      else pure $ B.concat blocks

  writeToFileAtOffset (B.unpack outputPath) (fromIntegral $ pieceIndex * pieceLength) piece

getReadyHandle :: (ByteString, ByteString) -> ByteString -> IO Handle
getReadyHandle peerAddress infoHash = do
  handle <- getSocketHandle (fst peerAddress) (snd peerAddress) infoHash
  _ <- B.hGet handle 68 -- peer handshake
  waitForBitfield handle
  sendInterested handle
  waitForUnchoke handle
  return handle

downloadWorker :: TorrentType -> PieceQueue -> (ByteString, ByteString) -> IO ()
downloadWorker torrent pieceQueue peer = do
  maybePiece <- atomically $ do
    pieces <- readTVar pieceQueue
    case pieces of
      [] -> return Nothing -- Queue is empty
      (p : ps) -> do
        writeTVar pieceQueue ps
        return (Just p)
  case maybePiece of
    Nothing -> return () -- Exit when the queue is empty
    Just piece -> do
      handle <- getReadyHandle peer (infoHash torrent)
      output <- getPiece (outputPath torrent) handle piece (pieceLength torrent) (fileLength torrent)
      hClose handle
      case output of
        Just _ -> downloadWorker torrent pieceQueue peer
        Nothing -> do
          atomically $ modifyTVar' pieceQueue (++ [piece]) -- Put the piece back in the queue if there is an error
          downloadWorker torrent pieceQueue peer

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
  x <- readTVar var
  writeTVar var $! f x

initializeQueue :: [Piece] -> IO PieceQueue
initializeQueue = newTVarIO

createFileIfNotExists :: FilePath -> IO ()
createFileIfNotExists path = do
  createDirectoryIfMissing True (takeDirectory path)
  handle <- openFile path WriteMode
  hClose handle -- Immediately close to clear the file content

takeDirectory :: FilePath -> FilePath
takeDirectory = reverse . dropWhile (/= '/') . reverse

downloadFile :: TorrentType -> IO Bool
downloadFile torrent = do
  let numPieces = fileLength torrent `div` pieceLength torrent
  let pieceIndices = [0 .. numPieces]
  queue <- initializeQueue pieceIndices
  createFileIfNotExists $ B.unpack (outputPath torrent)
  threads <- mapM (\peer -> async (downloadWorker torrent queue peer)) (peers torrent)
  mapM_ wait threads

  return True
