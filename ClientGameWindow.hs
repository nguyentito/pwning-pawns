import Control.Applicative
import Data.Foldable
import System.FilePath
import Text.Printf

import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Cairo
import Graphics.Rendering.Cairo

main :: IO ()
main = do
  initGUI
  Just xml <- xmlNew "client-game-window.glade"
  window <- xmlGetWidget xml castToWindow "window"
  canvas <- xmlGetWidget xml castToDrawingArea "canvas"
  widgetShowAll window
  withImageSurfacesFromPNGs (map ("fantasy-chess-pieces" </>)
                                 ["bk.png", "bq.png", "wk.png", "wq.png"]) $ \imgs -> do
    onExpose canvas $ const (True <$ updateCanvas canvas imgs)
    onButtonPress canvas handleButtonPress
    onDestroy window mainQuit
    mainGUI

withImageSurfacesFromPNGs :: [FilePath] -> ([Surface] -> IO a) -> IO a
withImageSurfacesFromPNGs [] act = act []
withImageSurfacesFromPNGs (fp:fps) act =
    withImageSurfaceFromPNG fp $ \img -> withImageSurfacesFromPNGs fps (act . (img:))

handleButtonPress :: Event -> IO Bool
handleButtonPress evt = do
  printf "%s mouse button pressed at (%f,%f)\n" btnDesc (eventX evt) (eventY evt)
  return True
      where btnDesc = case eventButton evt of
                        LeftButton -> "left"
                        RightButton -> "right"
                        _ -> "unknown"

updateCanvas :: DrawingArea -> [Surface] -> IO ()
updateCanvas canvas imgs = do
  (w, h) <- widgetGetSize canvas
  flip renderWithDrawable (drawBoard w h imgs) =<< widgetGetDrawWindow canvas

darkTileColor = (110, 128, 158)
lightTileColor = (209, 205, 184)

drawBoard :: Int -> Int -> [Surface] -> Render ()
drawBoard w h imgs = do
  let c = fromIntegral $ (min w h) - 2
  translate 1 1

  let (r,g,b) = darkTileColor in setSourceRGB (r/255) (g/255) (b/255)
  rectangle 0 0 c c
  fill
  
  let (r,g,b) = lightTileColor in setSourceRGB (r/255) (g/255) (b/255)
  let whiteSquares = map snd . filter fst
                     . zip (cycle (take 8 (cycle [True, False]) ++ take 8 (cycle [False, True])))
                     $ [ (x/8,y/8) | x <- [0..7], y <- [0..7] ]
  forM_ whiteSquares $ \(x, y) -> rectangle (x*c) (y*c) (c/8) (c/8)
  fill

  setSourceRGB 0 0 0
  setLineWidth 4
  forM_ [0..8] $ \i -> do
    moveTo 0 (i*c/8) >> lineTo c (i*c/8)
    moveTo (i*c/8) 0 >> lineTo (i*c/8) c
  stroke

  forM_ (zip [0..] imgs) $ \(i, img) -> do
    save
    translate (i*c/8) 0
    scale ((c/8)/920) ((c/8)/920)
    setSourceSurface img 0 0
    paint
    restore
