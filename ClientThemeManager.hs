module ClientThemeManager (startThemeManager,
                           withTheme,
                           Theme,
                           darkTileColor,
                           lightTileColor,
                           piecesImagesMap)
where

import Control.Applicative
import Control.Concurrent
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import System.FilePath
import System.IO.Unsafe

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal.Surfaces.PNG

import ChessTypes

-- Self-evident type declarations

type RGBTriple = (Double, Double, Double) -- the values must be in the 0-255 range
data Theme = Theme {
      darkTileColor   :: RGBTriple,
      lightTileColor  :: RGBTriple,
      piecesImagesMap :: Map Piece Surface
    }

-- Global variables (not exported, so their scope is limited to this file)
-- It's not very beautiful, but it's the simplest solution

themeMVar :: MVar Theme
-- Contains the theme data ; it also functions as a lock
-- to prevent using the data while the theme manager changes it
themeMVar = unsafePerformIO newEmptyMVar

newThemeChan :: Chan String -- To change the theme, write to this channel
newThemeChan = unsafePerformIO newChan

-- The theme manager is a loop running in its own thread, its only use is to manage
-- loading themes and managing the lifetime of the theme resources
--
-- To load a theme, the manager loop calls withThemeData
-- Until it recieves a request to load a new theme, the thread blocks inside the call to withTheme
-- and so the data loaded is still valid
-- When a theme change is requested, the the loop exits the withThemeData call
-- (freeing the resources of the old theme) and enters another iteration with the new theme
-- (blocking inside the new call to withThemeData, etc.)

startThemeManager :: String -> IO ()
startThemeManager defaultThemeName = do
  themeMVar `seq` newThemeChan `seq` forkIO $ loop defaultThemeName
  return ()
    where loop themeName = do
            newThemeName <- newIORef ""
            withThemeData themeName $ \theme -> do
              putMVar themeMVar theme -- theme data loaded : put the data in the MVar and release lock
              writeIORef newThemeName =<< waitForNewThemeRequest
              takeMVar themeMVar -- about to change theme data : acquire lock
            loop =<< readIORef newThemeName
          waitForNewThemeRequest = readChan newThemeChan -- readChan blocks until the chan recieves a message

-- This is how external functions access the current theme data

withTheme :: (Theme -> IO a) -> IO a
withTheme = withMVar themeMVar

-- This section deals with loading the themes

withThemeData :: String -> (Theme -> IO a) -> IO a
withThemeData themeID act = do
  -- withPiecesImagesFromDir ("themes" </> themeID)
  --   $ \pim -> act (Theme { darkTileColor = (110, 128, 158),
  --                          lightTileColor = (209, 205, 184),
  --                          piecesImagesMap = pim
  --                        }) <* putStrLn "baz"
  pim <- loadPiecesImagesFromDir $ "themes" </> themeID
  act $ Theme { darkTileColor = (110, 128, 158),
                lightTileColor = (209, 205, 184),
                piecesImagesMap = pim
              }
            
-- Unfortunately, using withImageSurfaceFromPNG creates bugs, so,
-- for now, we are going to use imageSurfaceCreateFromPNG and not care about
-- what happens when a new theme is loaded and the old images
-- become useless
withPiecesImagesFromDir :: FilePath -> (Map Piece Surface -> IO a) -> IO a
withPiecesImagesFromDir imgDir act = withImageSurfacesFromPNGs filenameList
                                     $ \surfaceList -> act (M.fromList (zip pieceList surfaceList))
    where pieceList = [ Piece piecetype color
                        | piecetype <- piecetypeList, color <- [White, Black]]
          piecetypeList = [King, Queen, Rook, Bishop, Knight, Pawn]
          filenameList = map pieceToFilename pieceList
          pieceToFilename (Piece piecetype color) = imgDir </> (letter1 : letter2 : ".png")
                  where letter1 = [(Black, 'b'), (White, 'w')] ! color
                        letter2 = (zip piecetypeList "kqrbnp") ! piecetype
          lst ! ix = fromJust $ lookup ix lst

loadPiecesImagesFromDir :: FilePath -> IO (Map Piece Surface)
loadPiecesImagesFromDir imgDir = do
  surfaceList <- mapM imageSurfaceCreateFromPNG filenameList
  return . M.fromList $ zip pieceList surfaceList
    where pieceList = [ Piece piecetype color
                        | piecetype <- piecetypeList, color <- [White, Black]]
          piecetypeList = [King, Queen, Rook, Bishop, Knight, Pawn]
          filenameList = map pieceToFilename pieceList
          pieceToFilename (Piece piecetype color) = imgDir </> (letter1 : letter2 : ".png")
                  where letter1 = [(Black, 'b'), (White, 'w')] ! color
                        letter2 = (zip piecetypeList "kqrbnp") ! piecetype
          lst ! ix = fromJust $ lookup ix lst

withImageSurfacesFromPNGs :: [FilePath] -> ([Surface] -> IO a) -> IO a
withImageSurfacesFromPNGs [] act = act []
withImageSurfacesFromPNGs (fp:fps) act =
    withImageSurfaceFromPNG fp $ \img -> withImageSurfacesFromPNGs fps (act . (img:))

-- A pretty GUI to change themes

