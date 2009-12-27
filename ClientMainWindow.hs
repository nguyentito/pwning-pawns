import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Network
import System.IO
import System.IO.Error

import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade

import qualified ListBox as LB

type GameID = Int

data GUI = GUI {
      mainWindow :: Window,
      btnQuit :: Button,
      btnConnect :: Button,
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
  mainGUI

runConnectDialog :: GUI -> MVar (Maybe Handle) -> IO ()
runConnectDialog gui connectionMVar = do
  let ok = dialogResponse (connectDialog gui) ResponseOk
      cancel = dialogResponse (connectDialog gui) ResponseCancel
  id1 <- onEntryActivate (cdServerEntry gui) ok
  id2 <- onClicked (cdBtnOk gui) ok
  id3 <- onClicked (cdBtnCancel gui) cancel
  response <- dialogRun (connectDialog gui)
  case response of
    ResponseOk -> do
      serverAddress <- get (cdServerEntry gui) entryText
      maybeHandle <- connectToServer serverAddress
      case maybeHandle of
        Nothing -> do
          msg <- messageDialogNew (Just (mainWindow gui)) []
                                  MessageError ButtonsClose
                                  ("Erreur : impossible de se connecter à " ++ serverAddress ++ ".")
          dialogRun msg
          widgetDestroy msg
          return ()
        Just handle -> do
          swapMVar connectionMVar (Just handle)
          set (btnConnect gui) [ buttonUseStock := True, buttonLabel := "gtk-disconnect" ]
          set (statusLabel gui) [ labelText := "Connecté à " ++ serverAddress ++ "." ]
    ResponseCancel -> return ()
  signalDisconnect id1
  signalDisconnect id2
  signalDisconnect id3
  widgetHide (connectDialog gui)
            
            
connectToServer :: HostName -> IO (Maybe Handle)
connectToServer serverAddress = do
  eitherHandle <- tryJust (guard . isDoesNotExistError)
                  $ connectTo serverAddress (PortNumber 28406)
  case eitherHandle of
    Left exn -> return Nothing
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

