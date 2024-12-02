{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick as BR
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as C
import qualified Brick.Widgets.Dialog as D
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import Peer (downloadFile)
import Torrent (readTorrentFile)

data Choice = Red | Blue | Green | White
  deriving (Show)

data Name
  = DecodeButton
  | InfoButton
  | PeersButton
  | WhiteButton
  deriving (Show, Eq, Ord)

newtype AppState = AppState
  { appContent :: [String]
  }

drawUI :: AppState -> [Widget Name]
drawUI state = [C.vBox [header, aside, footer]]
  where
    header = withAttr (attrName "headerAttr") $ C.hCenter $ C.str "monad-torrent"
    aside =
      C.hBox
        [ C.hLimitPercent 40 $ C.hBox [padAll 6 $ C.str "aside widget", vBorder],
          contentWidget
        ]
    vBorder = withAttr (attrName "borderAttr") B.vBorder
    contentWidget =
      C.vCenter $
        C.hCenter . padAll 2 $
          C.vBox $
            if Prelude.null (appContent state)
              then [C.str "No content found."]
              else
                [ C.hBox [C.padRight (C.Pad 2) (C.str line)]
                  | line <- appContent state
                ]
    footer =
      withAttr
        (attrName "footerAttr")
        $ C.hBox
          [ C.hCenter $ C.str "d - download File sample.torrent",
            C.hCenter $ C.str "p - peers",
            C.hCenter $ C.str "i - info",
            C.hCenter $ C.str "q - quit"
          ]

appEvent :: BrickEvent Name e -> EventM Name AppState ()
appEvent (VtyEvent ev) = do
  currentState <- get
  case ev of
    V.EvKey (V.KChar 'q') [] -> do
      BR.halt
    V.EvKey (V.KChar 'd') [] -> do
      parsedTorrentFile <- liftIO $ readTorrentFile "sample.torrent"
      result <- liftIO $ downloadFile parsedTorrentFile
      if result
        then modify $ \s -> s {appContent = ["File downloaded"]}
        else modify $ \s -> s {appContent = ["File download failed"]}
    _ -> do
      modify $ \s -> s {appContent = ["Invalid key", "please select a valid key"]}
appEvent _ = BR.continueWithoutRedraw

initialState :: AppState
initialState =
  AppState
    { appContent = ["Choose an action."] -- Default content
    }

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (D.dialogAttr, V.white `on` V.black),
      (D.buttonAttr, V.black `on` V.white),
      (attrName "headerAttr", V.black `on` V.white),
      (attrName "footerAttr", V.black `on` V.white),
      (attrName "borderAttr", V.white `on` V.black),
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

main :: IO ()
main = do
  _ <- BR.defaultMain theApp initialState
  putStrLn "Exiting..."