{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Main where

import Brick
import Brick.Forms
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.BChan
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.Socket
import Network.Run.TCP
import Control.Monad.IO.Class
import Control.Monad.Fix
import System.IO
import Data.Attoparsec.Text
import Data.Functor
import Control.Applicative
import Control.Concurrent

data Name = NameField
          | IpField
          | PortField
          | PeoplePort
          | MsgPort
          | EditorPort
  deriving (Eq, Ord, Show)

data TcpEvent = NewPerson T.Text
              | ByePerson T.Text
              | NewMsg T.Text T.Text
              | Quit

data ConnInfo = ConnInfo
  { _name :: T.Text
  , _ip   :: T.Text
  , _port :: Int
  } deriving Show
makeLenses ''ConnInfo

data ChatState = ChatState
  { _people   :: [T.Text]
  , _messages :: [(T.Text, T.Text)]
  , _edit     :: Editor T.Text Name
  }

makeLenses ''ChatState

data State e = ConfigDlg { _connForm  :: Form ConnInfo e Name, _stateChan :: BChan e, _mesgChan :: BChan T.Text }
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

mkEditor :: Editor T.Text Name
mkEditor = editorText EditorPort (Just 5) ""

draw :: State e -> [Widget Name]
draw (ConfigDlg formInfo _ _) = drawSetup formInfo
draw (ChatView chatstate)     = drawChat chatstate


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

messagesWindow :: [(T.Text, T.Text)] -> Widget Name
messagesWindow = borderWithLabel (str "Chat")
               . viewport MsgPort Vertical
               . vBox
               . map drawMessage
               . reverse


editorWindow :: Editor T.Text Name -> Widget Name
editorWindow = border . renderEditor (txt . T.unlines) True

drawMessage :: (T.Text,T.Text) -> Widget Name
drawMessage (per,msg) = hBox [ hLimit 10 $ padRight Max $ txt per
                             , txt msg
                             ]

drawChat :: ChatState -> [Widget Name]
drawChat cs = [(messagesWindow (cs ^. messages) <+> peopleWindow (cs ^. people)) <=> editorWindow (cs ^. edit)]


chooseCursor :: State e -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor (ConfigDlg f _ _) cl = focusRingCursor formFocus f cl
chooseCursor (ChatView cs)     cl = focusRingCursor (const $ focusRing [EditorPort]) cs cl

handleEvent :: BChan T.Text -> BrickEvent Name TcpEvent -> EventM Name (State TcpEvent) ()
handleEvent mChan ev = do
  st <- get
  case st of
    ConfigDlg {} -> formEvent ev
    ChatView _   -> zoom chatState $ chatEvent mChan ev

formEvent :: BrickEvent Name TcpEvent -> EventM Name (State TcpEvent) ()
formEvent (VtyEvent (V.EvResize {}))       = return ()
formEvent (VtyEvent (V.EvKey V.KEsc []))   = halt
formEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  case st of
    ConfigDlg form chan mChan -> connectSeg chan mChan $ formState form
    _ -> halt
formEvent ev = zoom connForm $ handleFormEvent ev

chatEvent :: BChan T.Text -> BrickEvent Name TcpEvent -> EventM Name ChatState ()
chatEvent mChan (VtyEvent (V.EvKey V.KEsc []))         = do
  liftIO $ writeBChan mChan "quit"
  return ()
chatEvent mChan (VtyEvent (V.EvKey V.KEnter []))       = do
  ed <- use edit
  let cont = T.unlines $ getEditContents ed
  liftIO $ writeBChan mChan cont
  edit .= mkEditor
chatEvent _     (AppEvent (NewPerson per))             = people %= (per:)
chatEvent _     (AppEvent (ByePerson per))             = people %= filter (/= per)
chatEvent _     (AppEvent (NewMsg per m))              = messages %= ((per,m):)
chatEvent _     (AppEvent Quit)                        = halt
chatEvent _     ev                                     = zoom edit $ handleEditorEvent ev

connectSeg :: BChan TcpEvent -> BChan T.Text -> ConnInfo -> EventM Name (State e) ()
connectSeg chan mChan dat = do
  liftIO $ void $ forkIO $ runTCPClient (T.unpack $ dat ^. ip) (show $ dat ^. port) (tcpClient chan mChan dat)
  let ed = mkEditor
  put $ ChatView $ ChatState [] [] ed

theMap :: AttrMap
theMap = attrMap V.defAttr []

main :: IO ()
main = do
  eventChan <- newBChan 10
  msgChan <- newBChan 10
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  let form = mkForm $ ConnInfo { _name = ""
                               , _ip   = "127.0.0.1"
                               , _port = 4242
                               }
  _ <- customMain initialVty buildVty (Just eventChan) (app msgChan) $ ConfigDlg form eventChan msgChan
  return ()

  where
    app c = App
      { appDraw = draw
      , appChooseCursor = chooseCursor
      , appHandleEvent  = handleEvent c
      , appStartEvent   = return ()
      , appAttrMap      = const theMap
      }

------------ Functionality -------------

data Message = AskName
             | Welcome
             | Instruction
             | PersonEntered T.Text
             | Message T.Text T.Text
             | PersonLeft T.Text
             | Online [T.Text]
             | Bye


askName :: Parser Message
askName = "Hi! What's your name?" $> AskName

welcome :: Parser Message
welcome = "Welcome " *> takeText $> Welcome

instruction :: Parser Message
instruction = "Type 'quit' to exit" $> Instruction

personEntered :: Parser Message
personEntered = PersonEntered <$> ("--> " *> takeTill isHorizontalSpace <* " entered chat.")

message' :: Parser Message
message' = liftA2 Message (takeTill (== ':') <* ": ") takeText

personLeft :: Parser Message
personLeft = PersonLeft <$> ("<-- " *> takeTill isHorizontalSpace <* " left.")

online :: Parser Message
online = Online <$> ("Users online: " *> (takeTill isHorizontalSpace `sepBy` char ' '))

bye :: Parser Message
bye = "Bye!" $> Bye

message :: Parser Message
message = askName <|> welcome <|> online <|> instruction <|> personEntered <|> message' <|> personLeft <|> bye

readMsg :: Handle -> IO Message
readMsg h = do
  msg <- T.hGetLine h
  either error return $ parseOnly message msg

writeMsg :: Handle -> T.Text -> IO ()
writeMsg = T.hPutStrLn

tcpClient :: BChan TcpEvent -> BChan T.Text -> ConnInfo -> Socket -> IO ()
tcpClient eChan mChan info sock = do
  hl <- socketToHandle sock ReadWriteMode
  hSetBuffering hl LineBuffering
  AskName <- readMsg hl
  writeMsg hl $ info ^. name
  Welcome <- readMsg hl
  (Online users) <- readMsg hl
  mapM_ (writeBChan eChan . NewPerson) users
  Instruction <- readMsg hl
  void $ forkIO $ fix $ \loop -> do
    msg <- readMsg hl
    case msg of
      PersonEntered per -> do
        writeBChan eChan (NewMsg "Server" $ T.concat ["-- ", per, " has joined --"])
        writeBChan eChan (NewPerson per)
        loop
      Message per m     -> writeBChan eChan (NewMsg per m)  >> loop
      PersonLeft per    -> do
        writeBChan eChan (NewMsg "Server" $ T.concat ["-- ", per, " has left --"])
        writeBChan eChan (ByePerson per)
        loop
      Bye               -> writeBChan eChan Quit
      _                 -> error "Unexpected msg"
  fix $ \loop -> do
    msg <- readBChan mChan
    if msg == "\n"
       then loop
       else do
        writeMsg hl msg
        writeBChan eChan $ NewMsg "you" msg
        loop
