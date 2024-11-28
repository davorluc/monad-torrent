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

data Choice = Red | Blue | Green | White
  deriving (Show)

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
          [ C.hCenter $ C.str "d - download piece",
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
    _ -> do
      updatedDialog <- nestEventM' (appDialog currentState) (D.handleDialogEvent ev)
      modify $ \s -> s {appDialog = updatedDialog}

initialState :: AppState
initialState =
  AppState
    { appDialog = D.dialog (Just $ str "Torrent operation") (Just (DecodeButton, choices)) 51,
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

doubleHorizontal :: BS.BorderStyle
doubleHorizontal =
  BS.BorderStyle
    { BS.bsCornerTL = '╒',
      BS.bsCornerTR = '╕',
      BS.bsCornerBR = '╛',
      BS.bsCornerBL = '╘',
      BS.bsIntersectL = '╞',
      BS.bsIntersectR = '╡',
      BS.bsIntersectT = '╤',
      BS.bsIntersectB = '╧',
      BS.bsIntersectFull = '╪',
      BS.bsHorizontal = '═',
      BS.bsVertical = '│'
    }

box3 :: Widget ()
box3 =
  C.freezeBorders $
    C.vBox
      [ C.hBox
          [ C.vLimit 4 B.vBorder,
            C.str "Resize horizontally to\nmove across the label\nbelow",
            C.vLimit 4 B.vBorder
          ],
        B.borderWithLabel (B.vBorder C.<+> C.str " Label " C.<+> B.vBorder) $
          C.hBox
            [ C.str "               ",
              C.vBox [B.vBorder, C.str "L\na\nb\ne\nl", C.vLimit 4 B.vBorder],
              C.str "\n\n\n Resize vertically to\n move across the label\n to the left\n\n\n\n\n" C.<=> B.hBorder
            ]
      ]

-- BYOB: build your own border
byob :: Widget ()
byob =
  C.vBox
    [ C.hBox [corner, top, corner],
      C.vLimit 7 $ C.hBox [B.vBorder, mid, B.vBorder],
      C.hBox [corner, B.hBorder, corner]
    ]
  where
    top = B.hBorderWithLabel (C.str "BYOB")
    mid = C.center (C.str "If `border` is too easy,\nyou can build it yourself")
    corner = B.joinableBorder (pure False)

ui :: Widget ()
ui = C.vBox [box3, byob]

main :: IO ()
main = do
  finalState <- BR.defaultMain theApp initialState
  putStrLn $ "You chose: " <> show (D.dialogSelection (appDialog finalState))
