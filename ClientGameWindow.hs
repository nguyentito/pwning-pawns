module ClientGameWindow (startGameWindow) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as M
import Text.Printf

import Graphics.UI.Gtk hiding (fill, Color)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Cairo
import Graphics.Rendering.Cairo

import ChessTypes
import Common

startGameWindow :: Chan String -> Chan String -> Color -> Map Piece Surface -> IO Window
startGameWindow playerMovesChan opponentMovesChan playerColor piecesImagesMap = do
  Just xml <- xmlNew "client-game-window.glade"
  window <- xmlGetWidget xml castToWindow "window"
  canvas <- xmlGetWidget xml castToDrawingArea "canvas"
  widgetShowAll window
  onExpose canvas $ const (True <$ updateCanvas canvas piecesImagesMap)
  onButtonPress canvas handleButtonPress
  return window

handleButtonPress :: Event -> IO Bool
handleButtonPress evt = do
  printf "%s mouse button pressed at (%f,%f)\n" btnDesc (eventX evt) (eventY evt)
  return True
      where btnDesc = case eventButton evt of
                        LeftButton -> "left"
                        RightButton -> "right"
                        _ -> "unknown"

updateCanvas :: DrawingArea -> Map Piece Surface -> IO ()
updateCanvas canvas piecesImagesMap = do
  (w, h) <- widgetGetSize canvas
  flip renderWithDrawable (drawBoard w h piecesImagesMap) =<< widgetGetDrawWindow canvas

darkTileColor = (110, 128, 158)
lightTileColor = (209, 205, 184)

drawBoard :: Int -> Int -> Map Piece Surface -> Render ()
drawBoard w h piecesImagesMap = do
  let c = fromIntegral $ (min w h) - 2
  translate 1 1

  let (r,g,b) = darkTileColor in setSourceRGB (r/255) (g/255) (b/255)
  rectangle 0 0 c c
  fill
  
  let (r,g,b) = lightTileColor in setSourceRGB (r/255) (g/255) (b/255)
  let lightSquares = map snd . filter fst
                     . zip (cycle (take 8 (cycle [True, False]) ++ take 8 (cycle [False, True])))
                     $ [ (x/8,y/8) | x <- [0..7], y <- [0..7] ]
  forM_ lightSquares $ \(x, y) -> rectangle (x*c) (y*c) (c/8) (c/8)
  fill

  setSourceRGB 0 0 0
  setLineWidth 4
  forM_ [0..8] $ \i -> do
    moveTo 0 (i*c/8) >> lineTo c (i*c/8)
    moveTo (i*c/8) 0 >> lineTo (i*c/8) c
  stroke

  drawPosition c piecesImagesMap startingPosition

drawPosition :: Double -> Map Piece Surface -> Position -> Render ()
drawPosition c piecesImagesMap (Position piecesMap _) =
    F.sequence_ . M.mapWithKey (drawPiece c piecesImagesMap) $ piecesMap

drawPiece :: Double -> Map Piece Surface -> Square -> Piece -> Render ()
drawPiece c piecesImagesMap (col, row) piece = do
  save
  translate ((fromIntegral col - 1) * c / 8) ((8 - fromIntegral row) * c / 8)
  scale ((c/8)/192) ((c/8)/192)
  setSourceSurface (piecesImagesMap M.! piece) 0 0
  paint
  restore
