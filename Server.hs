import Control.Concurrent
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.List
import System.Exit
import System.IO
import Network

import Util


data AppData = AppData {
      clientsMVar :: MVar [Handle],
      gamesMVar :: MVar [String]
    }


main :: IO ()
main = withSocketsDo $ do
  mvar1 <- newMVar []
  mvar2 <- newMVar []
  socket <- listenOn (PortNumber 28406)
  acceptClients socket (AppData { clientsMVar = mvar1, gamesMVar = mvar2 })

acceptClients :: Socket -> AppData -> IO ()
acceptClients socket appData = forever $ do
  (newClientHandle, _, _) <- accept socket
  hSetBuffering newClientHandle LineBuffering
  acceptClient newClientHandle appData

acceptClient :: Handle -> AppData -> IO ()
acceptClient newClientHandle appData = do
  msg <- hGetLine newClientHandle
  case words msg of
    ("CONNECT":_) -> do
      addClient newClientHandle appData
      hPutStrLn newClientHandle "CONNECTOK"
      sendGamesList newClientHandle appData
    _ -> do
      hClose newClientHandle

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
processMessagesFromClient clientHandle appData =
    processLinesFromHandle clientHandle dispatchAList
        where dispatchAList = [ ("CREATEGAME", createGame clientHandle appData) ]

createGame :: Handle -> AppData -> String ->  IO ()
createGame _ appData gameName = do
  modifyMVar_ (gamesMVar appData) $ \gamesList ->
      return (gameName:gamesList)
  withMVar (clientsMVar appData) $ mapM_ (flip sendGamesList appData)

sendGamesList :: Handle -> AppData -> IO ()
sendGamesList handle appData = do
  withMVar (gamesMVar appData) $ \gamesList ->
      hPutStrLn handle $ "GAMESLIST " ++ intercalate "\t" gamesList

