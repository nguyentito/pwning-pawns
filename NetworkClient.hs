import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import System.IO
import Network

import Graphics.UI.WX

import SDLClient
import Common
import ChessTypes
import Util


main :: IO ()
main = withSocketsDo . start $ do

  maybeHandleRef <- newIORef Nothing
  gamesMVar <- newMVar []
  gamesListUpdatedMVar <- newMVar False

  f <- frame [ text := "Pwning Pawns" ]
  connectionInfo <- staticText f [ text := "You are not connected." ]
  connectBtn <- button f []
  gamesListBox <- singleListBox f [ items := [] ]
  createGameBtn <- button f [ text := "Create game" ]
  joinGameBtn <- button f [ text := "Join game" ]
  quitBtn <- button f [ text := "Quit" ]
  set f [ layout :=  column 0 [ hfill $ row 0 [ floatCenter $ widget connectionInfo, widget connectBtn ],
                                fill $ widget gamesListBox,
                                hfill . row 0 $ map (fill . widget) [ createGameBtn, joinGameBtn, quitBtn ] ] ]

  set quitBtn [ on command := close f ]

  let connectBtnProps = [ text := "Connect", on command := onConnectCommand ]
      onConnectCommand = do
        ip <- textDialog f "Server address:" "Connect to server" ""
        maybeHandle <- connectToServer ip
        case maybeHandle of
          Nothing -> return ()
          Just handle -> do
            forkIO $ processIncomingMessages handle gamesMVar gamesListUpdatedMVar
            writeIORef maybeHandleRef (Just handle)
            set connectionInfo [ text := "Connected to server " ++ ip ++ "."]
            set connectBtn disconnectBtnProps

      disconnectBtnProps = [ text := "Disconnect", on command := onDisconnectCommand ]
      onDisconnectCommand = do
        maybe (return ()) hClose =<< readIORef maybeHandleRef
        writeIORef maybeHandleRef Nothing
        set connectionInfo [ text := "You are not connected" ]
        set connectBtn connectBtnProps 

  set connectBtn connectBtnProps

  set createGameBtn [ on command := do
                        maybeHandle <- readIORef maybeHandleRef
                        case maybeHandle of
                          Nothing -> return ()
                          Just handle -> do
                            createGameDialog f handle ]

  set joinGameBtn [ on command := return () ]
  
  let periodicRefresh = do
        modifyMVar_ (gamesListUpdatedMVar) $ \gamesListUpdated -> do
          when gamesListUpdated $ do
            withMVar gamesMVar $ \gamesList -> do
              set gamesListBox [ items := map snd gamesList ]
          return False
        yield
  timer f [ interval := 25, on command := periodicRefresh ]


connectToServer :: String -> IO (Maybe Handle)
connectToServer serverAddress = do
  handle <- connectTo serverAddress (PortNumber 28406)
  connectionError <- hIsClosed handle
  unlessMaybe connectionError $ do
    hSetBuffering handle LineBuffering
    hPutStrLn handle "CONNECT"
    response <- hGetLine handle
    unlessMaybe (response /= "CONNECTOK") $ do
      return $ Just handle

  
processIncomingMessages :: Handle -> MVar [(Int, String)] -> MVar Bool -> IO ()
processIncomingMessages handle gamesMVar gamesListUpdatedMVar=
    processLinesFromHandle handle dispatchAList
        where dispatchAList = [ ("GAMESLIST", refreshGamesList gamesMVar gamesListUpdatedMVar),
                                ("CREATEGAMEOK", const (return ())),
                                ("STARTGAME", startGame)]


refreshGamesList :: MVar [(Int, String)] -> MVar Bool -> String -> IO ()
refreshGamesList gamesMVar gamesListUpdatedMVar str = do
  swapMVar gamesMVar newGamesList
  swapMVar gamesListUpdatedMVar True
  return ()
      where newGamesList = map f . split '\t' $ str
            f gameStr = let (gameIDStr:_) = words gameStr in
                        (read gameIDStr, drop (length gameIDStr + 1) gameStr)


createGameDialog :: Frame a -> Handle -> IO ()
createGameDialog parentFrame handle = do
  d <- dialog parentFrame [ text := "Create Game" ]

  gameNameEntry <- textEntry d []
  colorChoice <- choice d [ items := ["White", "Black"] ]
  okBtn <- button d [ text := "OK" ]
  cancelBtn <- button d [ text := "Cancel" ]

  set d [ layout := grid 0 0 [ [ label "Name of the game:", hfill $ widget gameNameEntry ],
                               [ label "Your color:", hfill $ widget colorChoice ],
                               [ hfill hglue, widget okBtn, widget cancelBtn ] ] ]

  result <- showModal d (\stop -> do 
                           set okBtn [ on command := do
                                         gameName <- get gameNameEntry text
                                         colorNum <- get colorChoice selection
                                         stop $ Just (gameName, colorNum) ]
                           set cancelBtn [ on command := stop Nothing ] )
  
  case result of
    Nothing -> return ()
    Just (gameName, colorNum) -> createGame gameName ([White, Black] !! colorNum)


createGame :: Handle -> String -> Color -> IO ()
createGame gameName color handle = 
    hPutStrLn handle $ "CREATEGAME " ++ show color ++ " " ++ gameName


startGame :: String -> IO ()
startGame str = do
  undefined
    where [gameIDStr, colorStr] = words str
          gameID = read gameIDStr
          color = read colorStr


