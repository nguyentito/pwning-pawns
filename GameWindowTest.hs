import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import System.FilePath

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import ClientGameWindow
import ChessTypes

withImageSurfacesFromPNGs :: [FilePath] -> ([Surface] -> IO a) -> IO a
withImageSurfacesFromPNGs [] act = act []
withImageSurfacesFromPNGs (fp:fps) act =
    withImageSurfaceFromPNG fp $ \img -> withImageSurfacesFromPNGs fps (act . (img:))

withPiecesImages :: (Map Piece Surface -> IO a) -> IO a
withPiecesImages act = withImageSurfacesFromPNGs files
                       $ \surfaces -> act (M.fromList (zip pieces surfaces))
    where pieceTypes = [King, Queen, Rook, Bishop, Knight, Pawn]
          pieces = [Piece pieceType color | color <- [Black, White], pieceType <- pieceTypes]
          files = map pieceToFilePath pieces
          pieceToFilePath (Piece pieceType color) =
              "fantasy-chess-pieces"
              </> ((toLower . head . show $ color):
                   (fromJust $ lookup pieceType (zip pieceTypes "kqrbnp")):
                   ".png")

main = do
  initGUI
  playerMovesChan <- newChan
  opponentMovesChan <- newChan
  forkIO $ printMoves playerMovesChan
  withPiecesImages $ \piecesImagesMap -> do
    window <- startGameWindow playerMovesChan opponentMovesChan White piecesImagesMap
    onDestroy window mainQuit
    mainGUI

printMoves playerMovesChan = forever $ putStrLn =<< readChan playerMovesChan
