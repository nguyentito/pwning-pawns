import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import Network

import Graphics.UI.WX

import SDLClient
import Common
import ChessTypes


main :: IO ()
main = withSocketsDo . start $ do

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

  let connectBtnProps = [ text := "Connect", on command := onConnectCommand ]
      onConnectCommand = do
        ip <- textDialog f "Server address :" "Connect to server" ""
        maybeHandle <- connectToServer ip
        case maybeHandle of
          Nothing -> return ()
          Just handle -> do
            forkIO $ handleMessagesWithHeader "GAMESLIST" handle (refreshGamesList gamesMVar)
            set connectionInfo [ text := "Connected to server " ++ ip ++ "."]
            set connectBtn disconnectBtnProps

      disconnectBtnProps = [ text := "Disconnect", on command := onDisconnectCommand ]
      onDisconnectCommand = do
        set connectionInfo [ text := "You are not connected" ]
        set connectBtn connectBtnProps 

  set connectBtn connectBtnProps

  set quitBtn [ on command := close f ]
  set createGameBtn [ on command := return () ]
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
  
unlessMaybe :: (Monad m) => Bool -> m (Maybe a) -> m (Maybe a)
unlessMaybe True  _   = return Nothing
unlessMaybe False act = act


messageStream :: Handle -> IO [String]
messageStream handle = lines <$> hGetContents handle

handleMessagesWithHeader :: String -> Handle -> (String -> IO ()) -> IO ()
handleMessagesWithHeader header handle act =
    mapM_ act . mapMaybe (stripPrefix (header ++ " ")) =<< messageStream handle 


refreshGamesList :: MVar [String] -> String -> IO ()
refreshGamesList gamesMVar str = do
  modifyMVar_ gamesMVar $ const (return $ split '\t' str)
    where split _     [] = []
          split delim cs = case span (/= delim) cs of
                             (prefix, []) -> [prefix]
                             (prefix, (_:rest)) -> prefix : split delim rest


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

