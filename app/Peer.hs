module Peer
  ( getSocketHandle,
    waitForBitfield,
    waitForUnchoke,
    sendInterested,
    getPiece,
    downloadFile
  )
where

import Control.Concurrent.Async (async, wait)
import Control.Monad
import Data.Bits (shiftL)
import Data.ByteString (ByteString, foldl', hPut)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (chr, ord)
import Data.Word (Word8)
import Decoder (calculateHash, intToHexByteString, padWithZeros)
import GHC.Conc.Sync
import GHC.IO.Handle
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
import Brick (put, modify)
import qualified Data.ByteString as Bl
import Debug.Trace (trace, traceShowM)
import Control.Exception (IOException, catch)
import Control.Exception.Base (try)
import GHC.IO.IOMode

type Piece = Int -- Represent a piece by its index

type PieceQueue = TVar [Piece] -- A thread-safe queue of pieces

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


-- Attempt to open a binary file and handle the error if it is locked
openFileMaybe :: FilePath -> IOMode -> IO (Maybe Handle)
openFileMaybe path mode = do
  result <- try (openBinaryFile path mode) :: IO (Either IOException Handle)
  case result of
    Left _  -> return Nothing  -- If an IOException occurs, return Nothing
    Right h -> return (Just h) -- Otherwise, return Just Handle

writeToFileAtOffset :: FilePath -> Integer -> ByteString -> IO (Maybe ())
writeToFileAtOffset path offset content = do
  -- Check if the file exists
  handle <- openFileMaybe path ReadWriteMode -- Open in read-write mode if it exists
  case handle of
    Nothing -> do
      putStrLn $ "Failed to open " ++ path ++ " for writing"
      return Nothing
    Just h -> do
      -- Ensure the file is at least `offset` bytes long
      currentSize <- hFileSize h
      when (currentSize < offset) $ hSetFileSize h offset -- Do nothing if the size is sufficient
        -- Seek to the specified byte offset
      hSeek h AbsoluteSeek offset
      -- Write content to the file
      hPut h content
      -- Close the handle
      hClose h
      putStrLn $ "Data written to " ++ path ++ " at offset " ++ show offset
      return (Just ())
  -- Ensure the file is at least `offset` bytes long
  -- currentSize <- hFileSize handle
  -- if currentSize < offset
  --   then hSetFileSize handle offset
  --   else pure () -- Do nothing if the size is sufficient
  --   -- Seek to the specified byte offset
  -- hSeek handle AbsoluteSeek offset
  -- -- Write content to the file
  -- hPut handle content
  -- -- Close the handle
  -- hClose handle
  -- putStrLn $ "Data written to " ++ path ++ " at offset " ++ show offset

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

  -- LB.writeFile (B.unpack $ filename torrent) (LB.fromStrict $ B.concat piecesContent)
  -- write piece to a specific offset in the file
  -- with
  -- withFileAsInputStartingAt (fromIntegral $ pieceIndex * pieceLength) (B.unpack outputPath) $ \h -> do
  --   LB.hPut h (LB.fromStrict pieceData)
  putStrLn $ "Writing piece " <> show pieceIndex <> " to file " <> B.unpack outputPath

  writeToFileAtOffset (B.unpack outputPath) (fromIntegral $ pieceIndex * pieceLength) piece

getReadyHandle :: (ByteString, ByteString) -> ByteString -> IO Handle
getReadyHandle peerAddress infoHash = do
  handle <- getSocketHandle (fst peerAddress) (snd peerAddress) infoHash
  peerHandshakeResponse <- B.hGet handle 68 -- peer handshake
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
  handle <- openFile path WriteMode
  hClose handle -- Immediately close to clear the file content
  putStrLn $ "File cleared: " ++ path


splitIntoChunks :: Int -> ByteString -> [ByteString]
splitIntoChunks chunkSize bs
  | B.null bs = []
  | otherwise =
      let (chunk, rest) = B.splitAt chunkSize bs
      in chunk : splitIntoChunks chunkSize rest


downloadFile :: TorrentType -> IO Bool
downloadFile torrent = do

  let numPieces = fileLength torrent `div` pieceLength torrent
  let pieceIndices = [0 .. numPieces]
  queue <- initializeQueue pieceIndices
  -- let numThreads = length (peers torrent)
  traceShowM $ "Downloading file with " ++ show numPieces ++ " pieces"
  createFileIfNotExists $ B.unpack (outputPath torrent)
  traceShowM "File created"
  threads <- mapM (\peer -> async (downloadWorker torrent queue peer)) (peers torrent)
  mapM_ wait threads
  putStrLn "All pieces downloaded!"
  -- putStrLn "Verifying file..."

  -- handle <- openBinaryFile (B.unpack $ outputPath torrent) ReadMode
  -- content <- LB.hGetContents handle
  -- hClose handle
  -- let piecesHashes = map calculateHash (splitIntoChunks (2^14) (LB.toStrict content))
  -- let piecesHashesHex = map B16.encode piecesHashes
  -- let piecesHashesExpected = (pieceHashes torrent)
  -- let piecesHashesExpectedHex = map B16.encode piecesHashesExpected

  -- if and $ zipWith (==) piecesHashesHex piecesHashesExpectedHex
  --   then return True
  --   else return False
  return True