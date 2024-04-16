{-# LANGUAGE RecordWildCards, TemplateHaskell, OverloadedStrings #-}

module Main where

import Brick
import Brick.Forms
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Lens.Micro
--import Lens.Micro.Mtl
import Lens.Micro.TH

import qualified Graphics.Vty as V
import qualified Data.Text as T


data Name = NameField
          | IpField
          | PortField
          | PeoplePort
          | MsgPort
          | EditorPort
  deriving (Eq, Ord, Show)

data ConnInfo = ConnInfo
  { _name :: T.Text
  , _ip   :: T.Text
  , _port :: Int
  } deriving Show
makeLenses ''ConnInfo

data ChatState = ChatState
  { _people   :: [T.Text]
  , _messages :: [T.Text]
  , _edit     :: Editor T.Text Name
  }

makeLenses ''ChatState

data State e = ConfigDlg { _connForm  :: Form ConnInfo e Name }
             | ChatView  { _chatState :: ChatState }

makeLenses ''State

mkForm :: ConnInfo -> Form ConnInfo e Name
mkForm = let label s w = padBottom (Pad 1) $
                         vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
          in newForm 
              [ label "Name" @@= editTextField name NameField (Just 1)
              , label "IP"   @@= editTextField ip   IpField (Just 1)
              , label "Port" @@= editShowableField port PortField
              ]

draw :: State e -> [Widget Name]
draw (ConfigDlg formInfo) = drawSetup formInfo
draw (ChatView chatstate) = drawChat chatstate


drawSetup :: Form ConnInfo e Name -> [Widget Name]
drawSetup f = [center $ hLimit 50 $ borderWithLabel (str "Config") form <=> hCenter help]
  where form = padTop (Pad 1) $ renderForm f
        help = str "[Tab]: Change Field\n[Enter]: Submit\n[Esc]: Quit app"

peopleWindow :: [T.Text] -> Widget Name
peopleWindow = hLimit 20 
             . borderWithLabel (str "Online")
             . viewport PeoplePort Vertical 
             . vBox 
             . map txt

messagesWindow :: [T.Text] -> Widget Name
messagesWindow = borderWithLabel (str "Chat")
               . viewport MsgPort Vertical
               . vBox
               . map txt


editorWindow :: Editor T.Text Name -> Widget Name
editorWindow = border . renderEditor (txt . T.unlines) True

drawChat :: ChatState -> [Widget Name]
drawChat cs = [(messagesWindow (cs ^. messages) <+> peopleWindow (cs ^. people)) <=> editorWindow (cs ^. edit)]


chooseCursor :: State e -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor (ConfigDlg f) cl = focusRingCursor formFocus f cl
chooseCursor (ChatView cs) cl = focusRingCursor (const $ focusRing [EditorPort]) cs cl

handleEvent :: BrickEvent Name e -> EventM Name (State e) ()
handleEvent ev = do
  st <- get
  case st of
    ConfigDlg _ -> formEvent ev
    ChatView _  -> zoom chatState $ chatEvent ev

formEvent :: BrickEvent Name e -> EventM Name (State e) ()
formEvent (VtyEvent (V.EvResize {}))       = return ()
formEvent (VtyEvent (V.EvKey V.KEsc []))   = halt
formEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  case st of
    ConfigDlg form -> connectSeg $ formState form
    _ -> halt
formEvent ev = zoom connForm $ handleFormEvent ev

chatEvent :: BrickEvent Name e -> EventM Name ChatState ()
chatEvent (VtyEvent (V.EvKey V.KEsc []))         = halt
chatEvent (VtyEvent (V.EvKey V.KEnter []))       = halt
chatEvent ev                                     = zoom edit $ handleEditorEvent ev

connectSeg :: ConnInfo -> EventM Name (State e) ()
connectSeg dat = do
  let ed = editorText EditorPort (Just 5) ""
  put $ ChatView $ ChatState [dat ^. name] [] ed

theMap :: AttrMap
theMap = attrMap V.defAttr []

main :: IO ()
main = do
  let form = mkForm $ ConnInfo { _name = ""
                               , _ip   = ""
                               , _port = 0
                               }
  _ <- defaultMain app $ ConfigDlg form
  return ()

  where
    app = App{..}
    appDraw = draw
    appChooseCursor = chooseCursor
    appHandleEvent = handleEvent
    appStartEvent = return ()
    appAttrMap = const theMap
