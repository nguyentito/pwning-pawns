import Control.Concurrent
import Control.Monad

import SDLClient

main = do
  chan <- newChan
  forkIO $ forever (writeChan chan =<< getLine)
  mainSDL chan
