import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Network
import System.IO
import System.IO.Error

import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade

import qualified ListBox as LB
import Util
import ChessTypes
import ClientGameWindow

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
      cdBtnCancel :: Button,
      createGameDialog :: Dialog,
      cgdGameNameEntry :: Entry,
      cgdColorComboBox :: ComboBox,
      cgdBtnOk :: Button,
      cgdBtnCancel :: Button
    }

loadGUI :: IO GUI
loadGUI = do
  Just xml <- xmlNew "client-main-window.glade"
  listBox <- flip LB.newListBox [] =<< xmlGetWidget xml castToTreeView "treeView"
  GUI <$> xmlGetWidget xml castToWindow   "mainWindow"
      <*> xmlGetWidget xml castToButton   "btnQuit"
      <*> xmlGetWidget xml castToButton   "btnConnect"
      <*> xmlGetWidget xml castToButton   "btnCreate"
      <*> xmlGetWidget xml castToButton   "btnJoin"
      <*> return listBox
      <*> xmlGetWidget xml castToLabel    "statusLabel"
      <*> xmlGetWidget xml castToDialog   "connectDialog"
      <*> xmlGetWidget xml castToEntry    "cdServerEntry"
      <*> xmlGetWidget xml castToButton   "cdBtnOk"
      <*> xmlGetWidget xml castToButton   "cdBtnCancel"
      <*> xmlGetWidget xml castToDialog   "createGameDialog"
      <*> xmlGetWidget xml castToEntry    "cgdGameNameEntry"
      <*> xmlGetWidget xml castToComboBox "cgdColorComboBox"
      <*> xmlGetWidget xml castToButton   "cgdBtnOk"
      <*> xmlGetWidget xml castToButton   "cgdBtnCancel"

main :: IO ()
main = withSocketsDo . withGameWindowResourcesLoaded $ do
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
  onClicked (btnCreate gui) $ runCreateDialog connectionMVar gui
  onClicked (btnJoin gui) $ joinGame connectionMVar gui
  timeoutAddFull (True <$ yield) priorityDefaultIdle 100
  mainGUI

runOkCancelDialog :: Dialog -> Button -> Button -> IO () -> IO ()
runOkCancelDialog dialog okBtn cancelBtn act = do
  -- All the established signal connections are registered and disconnected at the end,
  -- to prevent a signal being connected twice after runConnectDialog runs twice.
  idOk <- onClicked okBtn $ dialogResponse dialog ResponseOk
  idCancel <- onClicked cancelBtn $ dialogResponse dialog ResponseCancel
  response <- dialogRun dialog
  when (response == ResponseOk) act
  signalDisconnect idOk
  signalDisconnect idCancel
  widgetHide dialog

runConnectDialog :: GUI -> MVar (Maybe Handle) -> IO ()
runConnectDialog gui connectionMVar = do
  id <- onEntryActivate (cdServerEntry gui) $ dialogResponse (connectDialog gui) ResponseOk
  runOkCancelDialog (connectDialog gui) (cdBtnOk gui) (cdBtnCancel gui) $ do
    success <- connect gui connectionMVar
    unless success $ do
      serverAddress <- get (cdServerEntry gui) entryText
      msg <- messageDialogNew (Just (mainWindow gui)) []
                              MessageError ButtonsClose
                              ("Erreur : impossible de se connecter à " ++ serverAddress ++ ".")
      dialogRun msg >> widgetDestroy msg >> return ()

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
    withMVar connectionMVar $ maybe (return ()) act

processMessagesFromServer :: Handle -> GUI -> IO ()
processMessagesFromServer serverHandle gui = do
  waitingGamesDialogsMVar <- newMVar M.empty
  playingGamesChansMVar <- newMVar M.empty
  processLinesFromHandle serverHandle [
      ("GAMESLIST"   , updateGamesList gui),
      ("CREATEGAMEOK", waitForGame gui waitingGamesDialogsMVar),
      ("STARTGAME"   , startGame waitingGamesDialogsMVar
                                 playingGamesChansMVar
                                 serverHandle),
      ("MOVE"        , handleOpponentMove playingGamesChansMVar)
    ]

runCreateDialog :: MVar (Maybe Handle) -> GUI -> IO ()
runCreateDialog connectionMVar gui = do
  set (cgdColorComboBox gui) [ comboBoxActive := 0 ]
  runOkCancelDialog (createGameDialog gui) (cgdBtnOk gui) (cgdBtnCancel gui) $ do
    gameName <- get (cgdGameNameEntry gui) entryText
    colorStr <- (["White", "Black"] !!) <$> get (cgdColorComboBox gui) comboBoxActive
    withConnectionHandle connectionMVar $ \handle -> do
      hPutStrLn handle $ "CREATEGAME " ++ colorStr ++ " " ++ gameName

updateGamesList :: GUI -> String -> IO ()
updateGamesList gui str = LB.setList (gamesListBox gui) (parse str)
    where parse = map (first read . second tail . span (/=' ')) . split '\t'

waitForGame :: GUI -> MVar (Map GameID MessageDialog) -> String -> IO ()
waitForGame gui waitingGamesDialogsMVar str = do
  dialog <- messageDialogNew (Just (mainWindow gui)) []
                             MessageInfo ButtonsNone
                             text
  widgetShowAll dialog
  modifyMVar_ waitingGamesDialogsMVar $ return . M.insert gameID dialog
      where gameID = read str
            text = "En attente du démarrage de la partie no." ++ show gameID ++ "..."

joinGame :: MVar (Maybe Handle) -> GUI -> IO ()
joinGame connectionMVar gui = do
  gameSelection <- LB.getSelection (gamesListBox gui)
  when (not (null gameSelection)) $ do
    let selectedGameID = head gameSelection
    withConnectionHandle connectionMVar $ \handle -> do
      hPutStrLn handle $ "JOINGAME " ++ show selectedGameID

startGame :: MVar (Map GameID MessageDialog)
          -> MVar (Map GameID (Chan String))
          -> Handle
          -> String
          -> IO ()
startGame waitingGamesDialogsMVar playingGamesChansMVar serverHandle str = do
  modifyMVar_ waitingGamesDialogsMVar $ \waitingGamesDialogsMap -> do
    maybe (return ()) (\dialog -> widgetDestroy dialog) $ M.lookup gameID waitingGamesDialogsMap
    return $ M.delete gameID waitingGamesDialogsMap
  playerMovesChan <- newChan
  opponentMovesChan <- newChan
  modifyMVar_ playingGamesChansMVar $ return . M.insert gameID opponentMovesChan
  forkIO $ handlePlayerMoves gameID playerMovesChan serverHandle
  startGameWindow playerMovesChan opponentMovesChan playerColor 
  return ()
    where [gameIDStr, colorStr] = words str
          playerColor = read colorStr
          gameID = read gameIDStr

handlePlayerMoves :: GameID -> (Chan String) -> Handle -> IO ()
handlePlayerMoves gameID playerMovesChan serverHandle =
    forever $ hPutStrLn serverHandle . moveToMsg =<< readChan playerMovesChan
        where moveToMsg moveStr = "MOVE " ++ show gameID ++ " " ++ moveStr

handleOpponentMove :: MVar (Map GameID (Chan String)) -> String -> IO ()
handleOpponentMove playingGamesChansMVar str = do
  withMVar playingGamesChansMVar $ \playingGamesChanMap -> do
    case M.lookup gameID playingGamesChanMap of
      Nothing -> return ()
      Just opponentMovesChan -> writeChan opponentMovesChan moveStr
  where
    (gameIDStr, ' ':moveStr) = span (/=' ') str
    gameID = read gameIDStr
      
