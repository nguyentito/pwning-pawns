import Control.Concurrent
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import System.Exit
import System.IO
import Network

import ChessTypes
import Common
import Util


-- Type declarations --
-----------------------

-- A record that gets passed to most functions
data AppData = AppData {
      clientsMVar :: MVar [Handle],
      waitingGamesMVar :: MVar (Map GameID WaitingGame),
      playingGamesMVar :: MVar (Map GameID PlayingGame),
      maxGameIDMVar :: MVar GameID
    }

type GameID = Int

-- Games created but not joined yet by a second player
data WaitingGame = WaitingGame {
      creatorHandle :: Handle,
      creatorColor :: Color,
      waitingGameName :: String
    }

-- Games currently playing
data PlayingGame = PlayingGame {
      playingGameName :: String,
      playersHandles :: [Handle]
    }


-- The superstructure : the main action + managing connections from clients --
------------------------------------------------------------------------------

-- Main action : just init everything and start accepting clients
main :: IO ()
main = withSocketsDo $ do
  emptyListMVar <- newMVar []
  emptyMapMVar1 <- newMVar M.empty
  emptyMapMVar2 <- newMVar M.empty
  zeroMVar <- newMVar 0

  socket <- listenOn (PortNumber 28406)
  acceptClients socket $ AppData {
                         clientsMVar = emptyListMVar,
                         waitingGamesMVar = emptyMapMVar1,
                         playingGamesMVar = emptyMapMVar2,
                         maxGameIDMVar = zeroMVar
                       }


-- A loop which accepts connections on the socket and delegates the rest to acceptClient
acceptClients :: Socket -> AppData -> IO ()
acceptClients socket appData = forever $ do
  (newClientHandle, _, _) <- accept socket
  hSetBuffering newClientHandle LineBuffering
  acceptClient newClientHandle appData

-- To accept the client, check the connection is working, then add the client
-- This function may change to support usernames
acceptClient :: Handle -> AppData -> IO ()
acceptClient newClientHandle appData = do
  msg <- hGetLine newClientHandle
  case words msg of
    ("CONNECT":_) -> do
      addClient newClientHandle appData
      hPutStrLn newClientHandle "CONNECTOK"
      sendGamesListTo newClientHandle appData
    _ -> do
      hClose newClientHandle

-- 1. Add the client to the list of clients
-- 2. Start processing messages from the client in another thread
addClient :: Handle -> AppData -> IO ()
addClient newClientHandle appData = do
  modifyMVar_ (clientsMVar appData) $ \clients ->
      return (newClientHandle:clients)
  forkIO $ processMessagesFromClient newClientHandle appData
  return ()

deleteClient :: Handle -> AppData -> IO ()
deleteClient clientHandle appData = do
  modifyMVar_ (clientsMVar appData) $ \clients ->
      return (delete clientHandle clients)
  hClose clientHandle

processMessagesFromClient :: Handle -> AppData -> IO ()
processMessagesFromClient clientHandle appData = do
  processLinesFromHandle clientHandle dispatchAList
  deleteClient clientHandle appData
      where dispatchAList =
                [("CREATEGAME", createGame clientHandle appData),
                 ("JOINGAME"  , joinGame   clientHandle appData),
                 ("MOVE"      , handleMove clientHandle appData) ]

handleMove = undefined

-- The actual request handling --
---------------------------------

sendGamesListTo :: Handle -> AppData -> IO ()
sendGamesListTo handle appData = do
  withMVar (waitingGamesMVar appData) $ \gamesMap -> do
    let msg = intercalate "\t" strList
        strList = map gameToStr .
                  filter ((handle /=) . creatorHandle . snd) .
                  M.toList $ gamesMap
        gameToStr (gameID, game) = show gameID ++ " " ++ waitingGameName game
    hPutStrLn handle $ "GAMESLIST " ++ msg

sendGamesList :: AppData -> IO ()
sendGamesList appData = do
  withMVar (clientsMVar appData) $ mapM_ (flip sendGamesListTo appData)

createGame :: Handle -> AppData -> String -> IO ()
createGame handle appData str = do
  let (Just (color, gameName)) = ( ((,) White) <$> stripPrefix "White " str )
                             <|> ( ((,) Black) <$> stripPrefix "Black " str )
  gameID <- createGame' handle color gameName appData
  hPutStrLn handle $ "CREATEGAMEOK " ++ show gameID
  sendGamesList appData

createGame' :: Handle -> Color -> String -> AppData -> IO GameID
createGame' handle color gameName appData = do
  modifyMVar (maxGameIDMVar appData) $ \maxGameID -> do
    let newGameID = maxGameID + 1
    modifyMVar_ (waitingGamesMVar appData) $
      return . M.insert newGameID (WaitingGame handle color gameName)
    return (newGameID, newGameID)

joinGame :: Handle -> AppData -> String -> IO ()
joinGame handle appData gameIDStr = do
  let gameID = read gameIDStr
  modifyMVar_ (waitingGamesMVar appData) $ \waitingGamesMap ->
    case M.lookup gameID waitingGamesMap of
      Nothing -> return waitingGamesMap
      Just game -> M.delete gameID waitingGamesMap <$ do
        modifyMVar_ (playingGamesMVar appData) $ \playingGamesMap -> do
          let playingGame = PlayingGame {
                              playingGameName = (waitingGameName game),
                              playersHandles = [creatorHandle game, handle]
                            }
              playerColorList = [(creatorColor game, creatorHandle game),
                                 (otherColor . creatorColor $ game, handle)]
          sendStartGame gameID playerColorList
          return (M.insert gameID playingGame playingGamesMap)

sendStartGame :: GameID -> [(Color, Handle)] -> IO ()
sendStartGame gameID = mapM_ f
    where f (color, handle) = hPutStrLn handle $
                              "STARTGAME " ++ show gameID ++ " " ++ show color

-- handleMove :: Handle -> AppData -> String -> IO ()
-- handleMove senderHandle appData str = do
--   let [gameIDStr, moveStr] = words str
--       gameID = read str
--   withMVar (playingGamesMVar appData) $ \playingGamesMap -> do
--     case M.lookup gameID playingGamesMap of
--       Nothing -> return ()
--       Just game -> do
--         let (_, opponentHandle) = M.findMin . M.filter (/= senderHandle) $ game
--         hPutStrLn opponentHandle str
    
    
