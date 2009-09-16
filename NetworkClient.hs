import Control.Concurrent
import Control.Monad
import Data.List
import System.IO
import Network

import SDLClient
import Common
import ChessTypes


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

