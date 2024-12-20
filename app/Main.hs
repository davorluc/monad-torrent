{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick as BR
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as C
import qualified Brick.Widgets.Dialog as D
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode, object, withObject, (.:), (.=))
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import qualified Graphics.Vty as V
import Peer (downloadFile)
import System.Directory as S
import Torrent (TorrentType (..), readTorrentFile)

data Choice = Red | Blue | Green | White
  deriving (Show)

data Name
  = DecodeButton
  | InfoButton
  | PeersButton
  | WhiteButton
  deriving (Show, Eq, Ord)

data AppState = AppState
  { appContent :: [String],
    textInputState :: TextInputState,
    showModal :: Bool,
    showKeybinds :: Bool,
    showSetting :: Bool,
    currentDownloadPath :: DownloadPathState,
    selectedTorrentIndex :: Int,
    torrents :: [TorrentType]
  }

data TextInputState = TextInputState
  { textInput :: String
  }

data DownloadPathState = DownloadPathState
  { downloadInput :: String
  }
  deriving (Show, Generic)

jsonFilePath :: FilePath
jsonFilePath = "torrents.json"

saveTorrentsToFile :: [TorrentType] -> IO ()
saveTorrentsToFile torrentList = BL.writeFile jsonFilePath (encode torrentList)

instance FromJSON DownloadPathState where
  parseJSON = withObject "DownloadPathState" $ \v -> do
    input <- v .: "downloadDirectory"
    return DownloadPathState {downloadInput = input}

instance ToJSON DownloadPathState where
  toJSON state =
    object
      ["downloadDirectory" .= downloadInput state]

drawUI :: AppState -> [Widget Name]
drawUI state =
  [C.vBox [header, aside, input, keybindsWidget, download, footer]]
  where
    header = withAttr (attrName "headerAttr") $ C.hCenter $ C.str "monad-torrent"
    aside =
      if showModal state || showKeybinds state || showSetting state
        then C.emptyWidget
        else
          C.hBox
            [ C.hLimitPercent 40 $
                C.hBox
                  [ C.vBox
                      [ styleEntry i (selectedTorrentIndex state) $
                          C.str $
                            B.unpack (fileName torrent)
                        | (i, torrent) <- Prelude.zip [0 ..] (torrents state)
                      ],
                    vBorder
                  ],
              contentWidget
            ]
    vBorder = withAttr (attrName "borderAttr") B.vBorder
    contentWidget =
      C.vCenter $
        C.hCenter . padAll 2 $
          C.vBox $
            if Prelude.null (torrents state)
              then [C.str "No content found."]
              else
                let selectedTorrent = torrents state !! selectedTorrentIndex state
                    torrentLines =
                      [ C.str $ "File Name: " ++ B.unpack (fileName selectedTorrent),
                        C.str $ "Output Path:" ++ B.unpack (outputPath selectedTorrent),
                        C.str $ "Info Hash: " ++ B.unpack (infoHash selectedTorrent),
                        C.str $ "File Length: " ++ show (fileLength selectedTorrent),
                        C.str $ "Piece Length: " ++ show (pieceLength selectedTorrent),
                        C.str $ "Tracker URL: " ++ B.unpack (trackerUrl selectedTorrent)
                      ]
                        ++ [C.str $ "Peer: " ++ B.unpack ip ++ ":" ++ B.unpack port | (ip, port) <- peers selectedTorrent]
                 in [ C.hBox [C.padRight (C.Pad 2) line]
                      | line <- torrentLines
                    ]
    input = modalWidget (showModal state) (textInputState state)
    keybindsWidget = keybindWidget (showKeybinds state)
    download = downloadPathWidget (showSetting state) (currentDownloadPath state)
    footer =
      withAttr
        (attrName "footerAttr")
        $ C.hBox
          [ C.hCenter $ C.str "h - help",
            C.hCenter $ C.str "q - quit"
          ]

appEvent :: BrickEvent Name e -> EventM Name AppState ()
appEvent (VtyEvent ev) = do
  currentState <- get
  if showSetting currentState
    then case ev of
      V.EvKey V.KEsc [] -> modify $ \s -> s {showSetting = False}
      V.EvKey V.KEnter [] -> do
        let newDownloadPath = downloadInput (currentDownloadPath currentState)
        liftIO $ BL.writeFile "settings.json" (encode (DownloadPathState newDownloadPath))
        modify $ \s -> s {showSetting = False, appContent = ["Download path updated successfully"]}
      V.EvKey V.KBS [] -> modify $ \s -> s {currentDownloadPath = (currentDownloadPath s) {downloadInput = Prelude.init (downloadInput (currentDownloadPath s))}}
      V.EvKey (V.KChar c) [] -> modify $ \s -> s {currentDownloadPath = (currentDownloadPath s) {downloadInput = downloadInput (currentDownloadPath s) ++ [c]}}
      _ -> BR.continueWithoutRedraw
    else
      if showModal currentState
        then case ev of
          V.EvKey V.KEsc [] -> modify $ \s -> s {showModal = False}
          V.EvKey V.KEnter [] -> do
            modify $ \s -> s {showModal = False}
            parsedTorrentFile <- liftIO $ readTorrentFile (B.pack (textInput (textInputState currentState)))
            result <- liftIO $ downloadFile parsedTorrentFile
            if result
              then do
                let newTorrent =
                      TorrentType
                        { fileName = fileName parsedTorrentFile,
                          outputPath = outputPath parsedTorrentFile,
                          infoHash = infoHash parsedTorrentFile,
                          pieceHashes = pieceHashes parsedTorrentFile,
                          fileLength = fileLength parsedTorrentFile,
                          pieceLength = pieceLength parsedTorrentFile,
                          trackerUrl = trackerUrl parsedTorrentFile,
                          peers = peers parsedTorrentFile
                        }
                modify $ \s -> s {torrents = newTorrent : torrents s}
                newState <- get
                liftIO $ saveTorrentsToFile (torrents newState)
              else modify $ \s -> s {appContent = ["File download failed"]}
          V.EvKey V.KBS [] -> modify $ \s -> s {textInputState = (textInputState s) {textInput = Prelude.init (textInput (textInputState s))}}
          V.EvKey (V.KChar c) [] -> modify $ \s -> s {textInputState = (textInputState s) {textInput = textInput (textInputState s) ++ [c]}}
          _ -> BR.continueWithoutRedraw
        else
          if showKeybinds currentState
            then case ev of
              V.EvKey V.KEsc [] -> modify $ \s -> s {showKeybinds = False}
              _ -> BR.continueWithoutRedraw
            else case ev of
              V.EvKey (V.KChar 'q') [] -> do
                BR.halt
              V.EvKey (V.KChar 'a') [] -> do
                modify $ \s -> s {showModal = not $ showModal s}
              V.EvKey V.KDown [] -> modify $ \s -> s {selectedTorrentIndex = clamp 0 (Prelude.length (torrents s) - 1) (selectedTorrentIndex s + 1)}
              V.EvKey (V.KChar 'j') [] -> modify $ \s -> s {selectedTorrentIndex = clamp 0 (Prelude.length (torrents s) - 1) (selectedTorrentIndex s + 1)}
              V.EvKey V.KUp [] -> modify $ \s -> s {selectedTorrentIndex = clamp 0 (Prelude.length (torrents s) - 1) (selectedTorrentIndex s - 1)}
              V.EvKey (V.KChar 'k') [] -> modify $ \s -> s {selectedTorrentIndex = clamp 0 (Prelude.length (torrents s) - 1) (selectedTorrentIndex s - 1)}
              V.EvKey (V.KChar 'd') [] -> do
                deleteState <- get
                let selectedIdx = selectedTorrentIndex deleteState
                let torrentList = torrents deleteState

                when (selectedIdx >= 0 && selectedIdx < Prelude.length torrentList) $ do
                  let torrentToDelete = torrentList !! selectedIdx
                  let filePathToDelete = B.unpack (outputPath torrentToDelete)

                  fileExists <- liftIO $ S.doesFileExist filePathToDelete
                  when fileExists $ liftIO $ S.removeFile filePathToDelete

                  let updatedTorrents = Prelude.take selectedIdx torrentList ++ Prelude.drop (selectedIdx + 1) torrentList
                  modify $ \s -> s {torrents = updatedTorrents, selectedTorrentIndex = clamp 0 (Prelude.length updatedTorrents - 1) selectedIdx}

                  liftIO $ saveTorrentsToFile updatedTorrents
              V.EvKey (V.KChar 'e') [] -> do
                expungeState <- get
                let selectedIdx = selectedTorrentIndex expungeState
                let torrentList = torrents expungeState

                when (selectedIdx >= 0 && selectedIdx < Prelude.length torrentList) $ do
                  let updatedTorrents = Prelude.take selectedIdx torrentList ++ Prelude.drop (selectedIdx + 1) torrentList
                  modify $ \s -> s {torrents = updatedTorrents, selectedTorrentIndex = clamp 0 (Prelude.length updatedTorrents - 1) selectedIdx}

                  liftIO $ saveTorrentsToFile updatedTorrents
              V.EvKey (V.KChar 'h') [] -> do
                modify $ \s -> s {showKeybinds = not $ showKeybinds s}
              V.EvKey (V.KChar 's') [] -> do
                modify $ \s -> s {showSetting = not $ showSetting s}
              _ -> do
                modify $ \s -> s {appContent = ["Invalid key", "please select a valid key"]}
appEvent _ = BR.continueWithoutRedraw

initialState :: IO AppState
initialState = do
  cwd <- S.getCurrentDirectory
  loadedTorrents <- loadTorrents jsonFilePath
  settings <- BL.readFile "settings.json"
  let settingsDecoded = case decode settings of
        Just (DownloadPathState {downloadInput = path}) -> path
        Nothing -> "./"
  let showSettingFlag = settingsDecoded == "./"
  return
    AppState
      { appContent = ["Choose an action."],
        textInputState = TextInputState {textInput = cwd <> "/"},
        torrents = loadedTorrents,
        showKeybinds = False,
        selectedTorrentIndex = 0,
        showSetting = showSettingFlag,
        currentDownloadPath = DownloadPathState {downloadInput = settingsDecoded},
        showModal = False
      }

styleEntry :: Int -> Int -> Widget Name -> Widget Name
styleEntry currentIndex targetIndex widget =
  if currentIndex == targetIndex
    then withAttr (attrName "entryAttr") widget
    else widget

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (D.dialogAttr, V.white `on` V.black),
      (D.buttonAttr, V.black `on` V.white),
      (attrName "headerAttr", V.black `on` V.white),
      (attrName "footerAttr", V.black `on` V.white),
      (attrName "borderAttr", V.white `on` V.black),
      (attrName "entryAttr", V.yellow `on` V.black),
      (attrName "borderAttrBlack", V.black `on` V.black),
      (attrName "inputAttr", V.white `on` V.black),
      (D.buttonSelectedAttr, bg V.yellow)
    ]

theApp :: BR.App AppState e Name
theApp =
  BR.App
    { BR.appDraw = drawUI,
      BR.appChooseCursor = BR.showFirstCursor,
      BR.appHandleEvent = appEvent,
      BR.appStartEvent = return (),
      BR.appAttrMap = const theMap
    }

modalWidget :: Bool -> TextInputState -> Widget Name
modalWidget False _ = C.emptyWidget
modalWidget True modalTextInputState =
  C.vCenter $
    C.hCenter $
      C.vLimitPercent 80 $
        C.hLimitPercent 90 $
          B.borderWithLabel (str "add torrent") $
            C.vBox
              [ modalHeader,
                C.vCenter $ C.hCenter $ C.hBox [C.hBox [textInputWidget modalTextInputState]],
                modalFooter
              ]
  where
    modalHeader = withAttr (attrName "borderAttrBlack") B.hBorder
    modalFooter = withAttr (attrName "borderAttrBlack") B.hBorder

textInputWidget :: TextInputState -> Widget Name
textInputWidget state =
  B.borderWithLabel (C.str "Filepath") $
    hLimitPercent 95 $
      C.vBox
        [ inputHeader,
          C.hBox
            [ C.withAttr (attrName "inputAttr") $
                C.vBox
                  [ C.str (textInput state),
                    inputFooter
                  ]
            ]
        ]
  where
    inputHeader = withAttr (attrName "borderAttr") B.hBorder
    inputFooter = withAttr (attrName "borderAttr") B.hBorder

keybindWidget :: Bool -> Widget Name
keybindWidget False = C.emptyWidget
keybindWidget True =
  C.vCenter $
    C.hCenter $
      C.vLimitPercent 80 $
        C.hLimitPercent 90 $
          B.borderWithLabel (str "keybinds") $
            C.vBox
              [ modalHeader,
                C.vCenter $ C.hCenter $ C.hBox [C.vBox [keybinds]],
                modalFooter
              ]
  where
    modalHeader = withAttr (attrName "borderAttrBlack") B.hBorder
    modalFooter = withAttr (attrName "borderAttrBlack") B.hBorder

keybinds :: Widget Name
keybinds =
  C.vBox
    [ C.str "a - add torrent: add a torrent to the download list",
      C.str "s - open settings",
      C.str "j/arrdown - down: navigate down",
      C.str "k/arrup - up: navigate up",
      C.str "d - delete file: delete file from monad-torrent list and file system",
      C.str "e - expunge file: delete file from monad-torrent list",
      C.str "h - help: shows this menu",
      C.str "esc - close an opened window",
      C.str "q - quit: quit the application"
    ]

downloadPathWidget :: Bool -> DownloadPathState -> Widget Name
downloadPathWidget False _ = C.emptyWidget
downloadPathWidget True path =
  C.vCenter $
    C.hCenter $
      C.vLimitPercent 80 $
        C.hLimitPercent 90 $
          B.borderWithLabel (str "Enter desired download directory") $
            C.vBox
              [ modalHeader,
                C.vCenter $ C.hCenter $ C.hBox [C.vBox [downloadPathInputWidget path]],
                modalFooter
              ]
  where
    modalHeader = withAttr (attrName "borderAttrBlack") B.hBorder
    modalFooter = withAttr (attrName "borderAttrBlack") B.hBorder

downloadPathInputWidget :: DownloadPathState -> Widget Name
downloadPathInputWidget state =
  B.borderWithLabel (C.str "download directory") $
    hLimitPercent 95 $
      C.vBox
        [ inputHeader,
          C.hBox
            [ C.withAttr (attrName "inputAttr") $
                C.vBox
                  [ C.str (downloadInput state),
                    inputFooter
                  ]
            ]
        ]
  where
    inputHeader = withAttr (attrName "borderAttr") B.hBorder
    inputFooter = withAttr (attrName "borderAttr") B.hBorder

loadTorrents :: FilePath -> IO [TorrentType]
loadTorrents filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      content <- BL.readFile filePath
      case decode content of
        Just torrentList -> return torrentList
        Nothing -> return []
    else return []

main :: IO ()
main = do
  initialisedState <- initialState
  _ <- BR.defaultMain theApp initialisedState
  Prelude.putStrLn "Exiting..."
