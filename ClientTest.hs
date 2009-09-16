import Control.Concurrent
import Control.Monad

import SDLClient
import ChessTypes

main = do
  guiMovesChan <- newChan
  cliMovesChan <- newChan
  forkIO $ forever (writeChan cliMovesChan =<< getLine)
  forkIO $ forever (putStrLn . ("You have played: " ++) =<< readChan guiMovesChan)
  mainSDL guiMovesChan cliMovesChan Black
