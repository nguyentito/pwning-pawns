import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad
import Network
import System.IO
import System.IO.Error

import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade

import qualified ListBox as LB
import Util

type GameID = Int

data GUI = GUI {
      mainWindow :: Window,
      btnQuit :: Button,
      btnConnect :: Button,
      btnCreate :: Button,
      btnJoin :: Button,
      gamesListBox :: LB.ListBox GameID,
      statusLabel :: Label,
      connectDialog :: Dialog,
      cdServerEntry :: Entry,
      cdBtnOk :: Button,
      cdBtnCancel :: Button
    }

loadGUI :: IO GUI
loadGUI = do
  Just xml <- xmlNew "client-main-window.glade"
  listBox <- flip LB.newListBox [] =<< xmlGetWidget xml castToTreeView "treeView"
  GUI <$> xmlGetWidget xml castToWindow "mainWindow"
      <*> xmlGetWidget xml castToButton "btnQuit"
      <*> xmlGetWidget xml castToButton "btnConnect"
      <*> xmlGetWidget xml castToButton "btnCreate"
      <*> xmlGetWidget xml castToButton "btnJoin"
      <*> return listBox
      <*> xmlGetWidget xml castToLabel "statusLabel"
      <*> xmlGetWidget xml castToDialog "connectDialog"
      <*> xmlGetWidget xml castToEntry  "cdServerEntry"
      <*> xmlGetWidget xml castToButton "cdBtnOk"
      <*> xmlGetWidget xml castToButton "cdBtnCancel"

main :: IO ()
main = withSocketsDo $ do
  connectionMVar <- newMVar Nothing
  initGUI
  gui <- loadGUI
  widgetShowAll (mainWindow gui)
  onDestroy (mainWindow gui) mainQuit
  onClicked (btnQuit gui) mainQuit
  onClicked (btnConnect gui) $ do
    maybeHandle <- readMVar connectionMVar
    case maybeHandle of
      Nothing -> runConnectDialog gui connectionMVar
      Just handle -> disconnect handle gui >> swapMVar connectionMVar Nothing >> return ()
  onClicked (btnCreate gui) $ createGame connectionMVar gui
  --  onClicked (btnJoin gui) $ joinGame connectionMVar gui
  timeoutAddFull (True <$ yield) priorityDefaultIdle 100
  mainGUI

runConnectDialog :: GUI -> MVar (Maybe Handle) -> IO ()
runConnectDialog gui connectionMVar = do
  let ok = dialogResponse (connectDialog gui) ResponseOk
      cancel = dialogResponse (connectDialog gui) ResponseCancel
  -- All the established signal connections are registered and disconnected at the end,
  -- to prevent a signal being connected twice after runConnectDialog runs twice.
  id1 <- onEntryActivate (cdServerEntry gui) ok
  id2 <- onClicked (cdBtnOk gui) ok
  id3 <- onClicked (cdBtnCancel gui) cancel
  response <- dialogRun (connectDialog gui)
  when (response == ResponseOk) $ do
    success <- connect gui connectionMVar
    unless success $ do
      serverAddress <- get (cdServerEntry gui) entryText
      msg <- messageDialogNew (Just (mainWindow gui)) []
                              MessageError ButtonsClose
                              ("Erreur : impossible de se connecter à " ++ serverAddress ++ ".")
      dialogRun msg >> widgetDestroy msg >> return ()
  signalDisconnect id1
  signalDisconnect id2
  signalDisconnect id3
  widgetHide (connectDialog gui)

connect :: GUI -> MVar (Maybe Handle) -> IO Bool
connect gui connectionMVar = do
  serverAddress <- get (cdServerEntry gui) entryText
  maybeHandle <- connectToServer serverAddress
  case maybeHandle of
    Nothing -> return False
    Just handle -> do
      swapMVar connectionMVar (Just handle)
      set (btnConnect gui) [ buttonUseStock := True, buttonLabel := "gtk-disconnect" ]
      set (statusLabel gui) [ labelText := "Connecté à " ++ serverAddress ++ "." ]
      forkIO $ processMessagesFromServer handle gui
      return True

connectToServer :: HostName -> IO (Maybe Handle)
connectToServer serverAddress = do
  eitherHandle <- tryJust (guard . isDoesNotExistError)
                  $ connectTo serverAddress (PortNumber 28406)
  case eitherHandle of
    Left _ -> return Nothing -- exception -> return a value which symbolizes error
    Right handle -> do
      hSetBuffering handle LineBuffering
      hPutStrLn handle "CONNECT "
      response <- hGetLine handle
      case response of
        "CONNECTOK" -> return $ Just handle
        _ -> return Nothing

disconnect :: Handle -> GUI -> IO ()
disconnect connectionHandle gui = do
  hClose connectionHandle
  set (btnConnect gui) [ buttonUseStock := True, buttonLabel := "gtk-connect" ]
  set (statusLabel gui) [ labelText := "Pas de connexion." ]

withConnectionHandle :: MVar (Maybe Handle) -> (Handle -> IO ()) -> IO ()
withConnectionHandle connectionMVar act = 
  withMVar connectionMVar $ \maybeHandle ->
    case maybeHandle of
      Nothing -> return ()
      Just handle -> act handle

processMessagesFromServer :: Handle -> GUI -> IO ()
processMessagesFromServer serverHandle gui = do
  processLinesFromHandle serverHandle dispatchAList
      where dispatchAList = [("GAMESLIST", updateGamesList gui)]

createGame :: MVar (Maybe Handle) -> GUI -> IO ()
createGame connectionMVar gui = do
  withConnectionHandle connectionMVar $ \handle -> do
    hPutStrLn handle "CREATEGAME White foobar"

updateGamesList :: GUI -> String -> IO ()
updateGamesList gui str = LB.setList (gamesListBox gui) (parse str)
    where parse = map (first read . second tail . span (/=' ')) . split '\t'

