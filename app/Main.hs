module Main (main) where

import Decoder
import Torrent
import Network.HTTP.Simple
import Network.Simple.TCP (connect)
import Network.Socket
import Control.Monad (void, when)
import System.Environment (getArgs)
import System.Exit
import System.IO
import Data.List (intercalate)
import Data.Map ((!))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Base16 as B16
import Peer

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) $ do
    putStrLn "Usage: your_bittorrent.sh <command> <args>"
    exitWith (ExitFailure 1)
  case args of
    ["decode", encodedValue] -> do
      let decodedValue = decodeBencodedValue (B.pack encodedValue)
      print decodedValue
    ["info", filePath] -> do
      fileContent <- LB.readFile filePath
      let json = decodedToDictionary $ decodeBencodedValue (B.concat $ LB.toChunks fileContent)
      let info = decodedToDictionary (json ! B.pack "info")
      putStrLn $ "Tracker URL: " ++ B.unpack (decodedToByteString (json ! B.pack "announce"))
      putStrLn $ "Length: " ++ show (info ! B.pack "length")
      putStrLn $ "Info Hash: " ++ B.unpack (calculateInfoHash (sortInfo info) "hex")
      putStrLn $ "Piece Length: " ++ show (info ! B.pack "piece length")
      putStrLn "Piece Hashes:"
      let pieces = getPieceHashes $ decodedToByteString (info ! B.pack "pieces")
      mapM_ (putStrLn . B.unpack . B16.encode) pieces
    ["peers", filePath] -> do
      let filePath = args !! 1
      fileContent <- LB.readFile filePath
      let json = decodedToDictionary $ decodeBencodedValue (B.concat $ LB.toChunks fileContent)
      let query = makeQuery json
      request <- parseRequest query
      response <- httpLBS request
      let peersBytes = decodedToDictionary (decodeBencodedValue $ LB.toStrict $ getResponseBody response) ! B.pack "peers"
      let peersList = peersToAddressList $ decodedToByteString peersBytes
      mapM_ (putStrLn . B.unpack) peersList
    ["handshake", filePath, peerAddress] -> do
      let peerIP = B.takeWhile (/= ':') $ B.pack peerAddress
      let peerPort = B.drop 1 $ B.dropWhile (/= ':') $ B.pack peerAddress
      fileContent <- LB.readFile filePath
      let json = decodedToDictionary $ decodeBencodedValue (B.concat $ LB.toChunks fileContent)
      let info = decodedToDictionary (json ! B.pack "info")
      let infoHash = calculateInfoHash (sortInfo info) ""
      handle <- getSocketHandle peerIP peerPort infoHash
      response <- B.hGet handle 68
      hClose handle
      let peerId = B.drop 48 response
      putStrLn $ "Peer ID: " ++ B.unpack (B16.encode peerId)
    ["download_piece", "-o", outputPath, filePath, pieceIndex] -> do
      fileContent <- LB.readFile filePath
      let json = decodedToDictionary $ decodeBencodedValue (B.concat $ LB.toChunks fileContent)
      let info = decodedToDictionary (json ! B.pack "info")
      let infoHash = calculateInfoHash (sortInfo info) ""
      let tracker_url = B.unpack (decodedToByteString (json ! B.pack "announce"))
      let query = makeQuery json
      request <- parseRequest query
      response <- httpLBS request
      let peersBytes = decodedToDictionary (decodeBencodedValue $ LB.toStrict $ getResponseBody response) ! B.pack "peers"
      let peersList = peersToAddressList $ decodedToByteString peersBytes
      let peerAddress = peersList !! 0
      let fileLength = read (show $ info ! B.pack "length") :: Int
      handle <- getSocketHandle (B.takeWhile (/= ':') peerAddress) (B.drop 1 $ B.dropWhile (/= ':') peerAddress) infoHash
      
      waitForBitfield handle
      -- putStrLn "Bitfield received!"

      sendInterested handle
      -- putStrLn "Interested sent!"

      waitForUnchoke handle

      let pieceLength = read (show $ info ! B.pack "piece length") :: Int
      print $ "pieceLength" <> show pieceLength
      print $ "fileLength" <> show fileLength
      getPiece handle (read pieceIndex) pieceLength outputPath fileLength

      putStrLn "Piece received!"

      hClose handle
    -- putStrLn "Piece downloaded!"
    _ -> putStrLn $ "Unknown command/sequence: " ++ intercalate " " args
