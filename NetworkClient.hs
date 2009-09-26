import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
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
            forkIO $ processIncomingMessages handle gamesMVar
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
                            gameName <- textDialog f "Name of the game:" "Create game" ""
                            createGame gameName handle ]

  set joinGameBtn [ on command := return () ]
  
  let periodicRefresh = do
        withMVar gamesMVar $ \gamesList ->
            set gamesListBox [ items := gamesList ]
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

  
processIncomingMessages :: Handle -> MVar [String] -> IO ()
processIncomingMessages handle gamesMVar =
    processLinesFromHandle handle dispatchAList
        where dispatchAList = [ ("GAMESLIST", refreshGamesList gamesMVar) ]

refreshGamesList :: MVar [String] -> String -> IO ()
refreshGamesList gamesMVar str = do
  modifyMVar_ gamesMVar $ const (return $ split '\t' str)


createGame :: String -> Handle -> IO ()
createGame gameName handle = hPutStrLn handle $ "CREATEGAME " ++ gameName ++ "\n"


{-
main :: IO ()
main = withSocketsDo $ do
  putStr "Server: " >> hFlush stdout
  hostname <- getLine
  handle <- connectTo hostname (PortNumber 28406)
  hSetBuffering handle LineBuffering
  hPutStrLn handle "CONNECT foobar"
  response <- hGetLine handle
  let playerColor = case response of
                      "ACCEPT W" -> White
                      "ACCEPT B" -> Black
                      _ -> error "Invalid response from server."
  putStrLn "Waiting for the game to begin..."
  startSignal <- hGetLine handle
  unless (startSignal == "START") $ error "Invalid response from server."
  startGame playerColor handle

startGame :: Color -> Handle -> IO ()
startGame playerColor handle = do
  playerMovesChan <- newChan
  opponentMovesChan <- newChan
  forkIO $ sendMoves handle playerMovesChan
  forkIO $ recieveMoves handle opponentMovesChan
  mainSDL playerMovesChan opponentMovesChan playerColor

sendMoves :: Handle -> Chan String -> IO ()
sendMoves handle chan = forever $ do
  moveStr <- readChan chan
  hPutStrLn handle $ "MOVE " ++ moveStr
  
recieveMoves :: Handle -> Chan String -> IO ()
recieveMoves handle chan = forever $ do
  str <- hGetLine handle
  when ("MOVE " `isPrefixOf` str) $ writeChan chan (drop 5 str)

-}

