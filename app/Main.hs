module Main (main) where

import Brick as BR
import qualified Brick as M
import qualified Brick.AttrMap as A
import qualified Brick.Types as T
import Brick.Util (bg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as C
import qualified Brick.Widgets.Dialog as D
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.List (intercalate)
import Data.Map as M
import Decoder
import qualified Graphics.Vty as V
import Network.HTTP.Simple
import Network.Simple.TCP (connect)
import Network.Socket
import Peer
import System.Environment (getArgs)
import System.Exit
import System.IO
import Torrent

-- Application State
type AppState = ()

data Name
  = DecodeButton
  | BlueButton
  | GreenButton
  | WhiteButton
  deriving (Show, Eq, Ord)

data AppState = AppState
  { appDialog :: D.Dialog Choice Name,
    appContent :: [String]
  }

drawUI :: AppState -> [Widget Name]
drawUI state = [ui]
  where
    contentWidget =
      C.hCenter . padAll 1 $
        C.vBox $
          if Prelude.null (appContent state)
            then [C.str "No content found."]
            else
              [ C.hBox [C.padRight (C.Pad 1) (C.str line)]
                | line <- appContent state
              ]
    ui = D.renderDialog (appDialog state) contentWidget

appEvent :: BrickEvent Name e -> EventM Name AppState ()
appEvent (VtyEvent ev) = do
  currentState <- get
  case ev of
    V.EvKey (V.KChar 'i') [] -> do
      torrentInfo <- liftIO loadTorrentInfo
      modify $ \s -> s {appContent = ["Torrent Info: " ++ torrentInfo]}
    V.EvKey (V.KChar 'p') [] -> do
      let filePath = "./sample.torrent"
      fileContent <- liftIO $ LB.readFile filePath
      let decoded = decodeBencodedValue (B.concat $ LB.toChunks fileContent)
      let json = decodedToDictionary decoded
      let query = makeQuery json
      request <- liftIO $ parseRequest query
      response <- liftIO $ httpLBS request
      let peersBytes = decodedToDictionary (decodeBencodedValue $ LB.toStrict $ getResponseBody response) ! B.pack "peers"
      let peersList = peersToAddressList $ decodedToByteString peersBytes
      let formattedPeers = zipWith (\i peer -> "Peer " ++ show i ++ ": " ++ B.unpack peer) [1 ..] peersList
      modify $ \s -> s {appContent = formattedPeers}
    V.EvKey (V.KChar 'q') [] -> do
      BR.halt
    _ -> do
      updatedDialog <- nestEventM' (appDialog currentState) (D.handleDialogEvent ev)
      modify $ \s -> s {appDialog = updatedDialog}

initialState :: AppState
initialState =
  AppState
    { appDialog = D.dialog (Just $ str "Torrent operation") (Just (DecodeButton, choices)) 50,
      appContent = ["Choose an action."] -- Default content
    }
  where
    choices =
      [ ("decode", DecodeButton, Red),
        ("info", BlueButton, Blue),
        ("peers", GreenButton, Green),
        ("handShake", WhiteButton, White),
        ("downloadPiece", WhiteButton, White)
      ]

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (D.dialogAttr, V.white `on` V.black),
      (D.buttonAttr, V.black `on` V.white),
      (D.buttonSelectedAttr, bg V.yellow)
    ]

theApp :: BR.App AppState e Name
theApp =
  BR.App
    { BR.appDraw = drawUI,
      BR.appChooseCursor = BR.showFirstCursor,
      BR.appHandleEvent = appEvent, -- Ensure correct capitalization
      BR.appStartEvent = return (),
      BR.appAttrMap = const theMap
    }

startEvent :: EventM n s ()
startEvent = return ()

-- Draw the UI
drawUI :: AppState -> [Widget ()]
drawUI _ =
  [ center $ str "Press 'q' to quit, 'm' for menu, 's' for save, 'Esc' to quit.",
    hBorder,
    center $ str "Bottom Menu: [q: Quit] [m: Menu] [s: Save] [Esc: Quit]"
  ]

-- Handle Events
handleEvent :: BrickEvent n e -> EventM n AppState ()
handleEvent (VtyEvent (EvKey (KChar 'q') [])) = halt
handleEvent (VtyEvent (EvKey KEsc [])) = halt
handleEvent (VtyEvent (EvKey (KChar 'm') [])) = do
  liftIO $ putStrLn "Menu key pressed!"
handleEvent (VtyEvent (EvKey (KChar 's') [])) = do
  liftIO $ putStrLn "Save key pressed!"
handleEvent _ = return ()

-- Attribute Map
theMap :: AttrMap
theMap = attrMap defAttr []

loadTorrentInfo :: IO String
loadTorrentInfo = do
  let filePath = "./sample.torrent"
  fileContent <- LB.readFile filePath
  let decoded = decodeBencodedValue (B.concat $ LB.toChunks fileContent)
  let json = decodedToDictionary decoded
  let info = decodedToDictionary (json M.! B.pack "info")
  let trackerUrl = B.unpack $ decodedToByteString (json M.! B.pack "announce")
  let pieceLength = show (info M.! B.pack "piece length")
  let fileLength = show (info M.! B.pack "length")
  return $
    unlines
      [ "Tracker URL: " ++ trackerUrl,
        "Piece Length: " ++ pieceLength,
        "File Length: " ++ fileLength
      ]

main :: IO ()
main = do
  finalState <- BR.defaultMain theApp initialState
  putStrLn $ "You chose: " <> show (D.dialogSelection (appDialog finalState))

-- do
-- args <- getArgs
-- when (length args < 2) $ do
--   putStrLn "Usage: your_bittorrent.sh <command> <args>"
--   exitWith (ExitFailure 1)
-- case args of
--   ["decode", encodedValue] -> do
--     let decodedValue = decodeBencodedValue (B.pack encodedValue)
--     print decodedValue
--   ["info", filePath] -> do
--     fileContent <- LB.readFile filePath
--     let json = decodedToDictionary $ decodeBencodedValue (B.concat $ LB.toChunks fileContent)
--     let info = decodedToDictionary (json ! B.pack "info")
--     putStrLn $ "Tracker URL: " ++ B.unpack (decodedToByteString (json ! B.pack "announce"))
--     putStrLn $ "Length: " ++ show (info ! B.pack "length")
--     putStrLn $ "Info Hash: " ++ B.unpack (calculateInfoHash (sortInfo info) "hex")
--     putStrLn $ "Piece Length: " ++ show (info ! B.pack "piece length")
--     putStrLn "Piece Hashes:"
--     let pieces = getPieceHashes $ decodedToByteString (info ! B.pack "pieces")
--     mapM_ (putStrLn . B.unpack . B16.encode) pieces
--   ["peers", filePath] -> do
--     let filePath = args !! 1
--     fileContent <- LB.readFile filePath
--     let json = decodedToDictionary $ decodeBencodedValue (B.concat $ LB.toChunks fileContent)
--     let query = makeQuery json
--     request <- parseRequest query
--     response <- httpLBS request
--     let peersBytes = decodedToDictionary (decodeBencodedValue $ LB.toStrict $ getResponseBody response) ! B.pack "peers"
--     let peersList = peersToAddressList $ decodedToByteString peersBytes
--     mapM_ (putStrLn . B.unpack) peersList
--   ["handshake", filePath, peerAddress] -> do
--     let peerIP = B.takeWhile (/= ':') $ B.pack peerAddress
--     let peerPort = B.drop 1 $ B.dropWhile (/= ':') $ B.pack peerAddress
--     fileContent <- LB.readFile filePath
--     let json = decodedToDictionary $ decodeBencodedValue (B.concat $ LB.toChunks fileContent)
--     let info = decodedToDictionary (json ! B.pack "info")
--     let infoHash = calculateInfoHash (sortInfo info) ""
--     handle <- getSocketHandle peerIP peerPort infoHash
--     response <- B.hGet handle 68
--     hClose handle
--     let peerId = B.drop 48 response
--     putStrLn $ "Peer ID: " ++ B.unpack (B16.encode peerId)
--   ["download_piece", "-o", outputPath, filePath, pieceIndex] -> do
--     fileContent <- LB.readFile filePath
--     let json = decodedToDictionary $ decodeBencodedValue (B.concat $ LB.toChunks fileContent)
--     let info = decodedToDictionary (json ! B.pack "info")
--     let infoHash = calculateInfoHash (sortInfo info) ""
--     let tracker_url = B.unpack (decodedToByteString (json ! B.pack "announce"))
--     let query = makeQuery json
--     request <- parseRequest query
--     response <- httpLBS request
--     let peersBytes = decodedToDictionary (decodeBencodedValue $ LB.toStrict $ getResponseBody response) ! B.pack "peers"
--     let peersList = peersToAddressList $ decodedToByteString peersBytes
--     let peerAddress = peersList !! 0
--     let fileLength = read (show $ info ! B.pack "length") :: Int
--     handle <- getSocketHandle (B.takeWhile (/= ':') peerAddress) (B.drop 1 $ B.dropWhile (/= ':') peerAddress) infoHash

--     waitForBitfield handle
--     -- putStrLn "Bitfield received!"

--     sendInterested handle
--     -- putStrLn "Interested sent!"

--     waitForUnchoke handle

--     let pieceLength = read (show $ info ! B.pack "piece length") :: Int
--     print $ "pieceLength" <> show pieceLength
--     print $ "fileLength" <> show fileLength
--     getPiece handle (read pieceIndex) pieceLength outputPath fileLength

--     putStrLn "Piece received!"

--     hClose handle
--   -- putStrLn "Piece downloaded!"
--   _ -> putStrLn $ "Unknown command/sequence: " ++ intercalate " " args
