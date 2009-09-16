import Control.Concurrent
import Control.Monad

import SDLClient
import ChessTypes

main = do
  whiteMovesChan <- newChan
  blackMovesChan <- newChan
  forkIO $ forever (writeChan blackMovesChan =<< getLine)
  forkIO $ forever (putStrLn . ("White has moved: " ++) =<< readChan whiteMovesChan)
  mainSDL whiteMovesChan blackMovesChan White
