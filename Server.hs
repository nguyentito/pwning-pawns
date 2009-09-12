import Control.Applicative
import System.Exit
import System.IO
import Network

main = withSocketsDo $ do
  socket <- listenOn (PortNumber 28406)
  whiteHandle <- acceptClient socket "W"
  blackHandle <- acceptClient socket "B"
  hPutStrLn whiteHandle "START"
  hPutStrLn blackHandle "START"
  forever $ do
    hPutStrLn blackHandle =<< hGetLine whiteHandle
    hPutStrLn whiteHandle =<< hGetLine blackHandle
    disconnected <- or <$> mapM hIsClosed [whiteHandle, blackHandle]
    when disconnected exitSuccess

acceptClient socket response = do
  (newClientHandle, _, _) <- accept socket
  hSetBuffering newClientHandle LineBuffering
  msg <- hGetLine clientHandle
  if ("CONNECT" `isPrefixOf` msg)
    then do
      hPutStrLn newClientHandle $ "ACCEPT " ++ response
      return newClientHandle
    else do
      hClose newClientHandle
      acceptClient socket response

      
