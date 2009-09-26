import Control.Concurrent
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.List
import System.Exit
import System.IO
import Network

------------------------------------------------------

data AppData = AppData {
      clientsMVar :: MVar [Handle],
      gamesMVar :: MVar [String]
    }

type AppMonad a = ReaderT AppData IO a

forkAppMonad :: AppMonad () -> AppMonad ThreadId
forkAppMonad act = liftIO . forkIO . runReaderT act =<< ask

------------------------------------------------------


main :: IO ()
main = withSocketsDo $ do
  mvar1 <- newMVar []
  mvar2 <- newMVar ["teletubbies", "tinky winky", "dipsy", "lala", "po"]
  socket <- listenOn (PortNumber 28406)
  runReaderT (acceptClients socket) (AppData { clientsMVar = mvar1, gamesMVar = mvar2 })

acceptClients :: Socket -> AppMonad ()
acceptClients socket = forever $ do
  (newClientHandle, _, _) <- liftIO $ accept socket
  liftIO $ hSetBuffering newClientHandle LineBuffering
  acceptClient newClientHandle

acceptClient :: Handle -> AppMonad ()
acceptClient newClientHandle = do
  msg <- liftIO $ hGetLine newClientHandle
  case words msg of
    ("CONNECT":_) -> do
      addClient newClientHandle
      liftIO $ hPutStrLn newClientHandle "CONNECTOK"
      sendGamesList newClientHandle
    _ -> do
      liftIO $ hClose newClientHandle

addClient :: Handle -> AppMonad ()
addClient newClientHandle = do
  cmv <- asks clientsMVar
  liftIO . modifyMVar_ cmv $ \clients ->
      return (newClientHandle:clients)
  forkAppMonad $ recvMsgLoop newClientHandle
  return ()

recvMsgLoop :: Handle -> AppMonad ()
recvMsgLoop clientHandle = do
  mapM_ (processMsg clientHandle) . lines =<< (liftIO $ hGetContents clientHandle)
  deleteClient clientHandle

deleteClient :: Handle -> AppMonad ()
deleteClient clientHandle = do
  cmv <- asks clientsMVar
  liftIO . modifyMVar_ cmv $ \clients ->
      return (delete clientHandle clients)
  liftIO $ hClose clientHandle

processMsg :: Handle -> String -> AppMonad ()
processMsg senderHandle msg =
    case words msg of
      _ -> return ()
--      ["CREATEGAME", gameName] -> createGame gameName senderHandle
--      _ -> undefined

createGame :: String -> Handle -> AppMonad ()
createGame = undefined

sendGamesList :: Handle -> AppMonad ()
sendGamesList handle = do
  gmv <- asks gamesMVar
  liftIO . withMVar gmv $ \gamesList ->
      hPutStrLn handle $ "GAMESLIST " ++ intercalate "\t" gamesList ++ "\n"

