module ClientGameWindow (startGameWindow) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as M
import System.FilePath
import Text.Printf

import Graphics.UI.Gtk hiding (fill, Color)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Cairo
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal.Surfaces.PNG

import AlgebraicNotation
import ChessTypes
import Common

-- Main --
----------

data AppData = AppData {
      piecesImagesMap :: Map Piece Surface,
      playerMovesChan :: Chan String,
      opponentMovesChan :: Chan String,
      playerColor :: Color,
      requestRedraw :: IO ()
    }

data AppState = AppState {
      position :: Position,
      selectedSquare :: Maybe Square,
      currentMovesMap :: Maybe (Map Square Move),
      sideToPlay :: Color
    }

startGameWindow :: Chan String -> Chan String -> Color -> IO Window
startGameWindow playerMovesChan opponentMovesChan playerColor = do
  Just xml <- xmlNew "client-game-window.glade"
  window <- xmlGetWidget xml castToWindow "window"
  canvas <- xmlGetWidget xml castToDrawingArea "canvas"
  widgetShowAll window
  piecesImagesMap <- loadPiecesImagesMap
  let appData = AppData piecesImagesMap playerMovesChan opponentMovesChan playerColor
                        (widgetQueueDraw canvas)
  stateRef <- newIORef $ AppState {
                position = startingPosition,
                selectedSquare = Nothing,
                currentMovesMap = Nothing,
                sideToPlay = White
              }
  onExpose canvas $ const (True <$ updateCanvas canvas appData stateRef)
  onButtonPress canvas $ handleButtonPress canvas appData stateRef
  timeoutAdd (handleOpponentMoves appData stateRef) 100
  return window

loadPiecesImagesMap :: IO (Map Piece Surface)
loadPiecesImagesMap = do
  surfaceList <- mapM imageSurfaceCreateFromPNG filenameList
  return $ M.fromList (zip pieceList surfaceList)
    where pieceList = [ Piece piecetype color
                        | piecetype <- piecetypeList, color <- [White, Black]]
          piecetypeList = [King, Queen, Rook, Bishop, Knight, Pawn]
          filenameList = map pieceToFilename pieceList
          pieceToFilename (Piece piecetype color) = 
              "fantasy-chess-pieces" </> (letter1 : letter2 : ".png")
                  where letter1 = [(Black, 'b'), (White, 'w')] ! color
                        letter2 = (zip piecetypeList "kqrbnp") ! piecetype
          lst ! ix = fromJust $ lookup ix lst
                        

-- Event handling --
--------------------

handleButtonPress :: DrawingArea -> AppData -> IORef AppState -> Event -> IO Bool
handleButtonPress canvas appData stateRef evt = do
  when (eventButton evt == LeftButton) $ do
    (w, h) <- widgetGetSize canvas
    let x = eventX evt
        y = eventY evt
        c = fromIntegral $ (min w h) - 2
        square = (floor ((x - 1) / (c/8)) + 1,
                  8 - floor ((y - 1) / (c/8)))
    onClickedSquare square appData stateRef
    requestRedraw appData
  return True

onClickedSquare :: Square -> AppData -> IORef AppState -> IO ()
onClickedSquare square@(col,row) appData stateRef = do
  appState <- readIORef stateRef
  when (sideToPlay appState == playerColor appData) $ do
    case selectedSquare appState of
      Just selSq | selSq == square -> unselectSquare
                 | otherwise -> case M.lookup square =<< currentMovesMap appState of
                                  Just move -> do applyPlayerMove move appData stateRef
                                                  unselectSquare
                                  Nothing -> return ()
      Nothing -> selectSquare (Just square) appData stateRef
    where unselectSquare = selectSquare Nothing appData stateRef

selectSquare :: Maybe Square -> AppData -> IORef AppState -> IO ()
selectSquare Nothing _ stateRef =
    modifyIORef stateRef $ \s -> s { selectedSquare = Nothing,
                                     currentMovesMap = Nothing }
selectSquare maybeSquare@(Just square) appData stateRef = do
  pos@(Position board _) <- position <$> readIORef stateRef
  case M.lookup square board of 
    Just piece@(Piece _ color)
        | color == playerColor appData ->
            let newMovesMap = legalMovesMap piece square pos in
            modifyIORef stateRef $ \s -> s { selectedSquare = maybeSquare,
                                             currentMovesMap = Just newMovesMap }
    _ -> return ()

applyPlayerMove :: Move -> AppData -> IORef AppState -> IO ()
applyPlayerMove move appData stateRef = do
  modifyIORef stateRef $ \s -> s { position = applyMove move (playerColor appData) (position s),
                                   sideToPlay = otherColor $ playerColor appData }
  writeChan (playerMovesChan appData) $ printMove move


-- Timer-triggered actions --
-----------------------------

handleOpponentMoves :: AppData -> IORef AppState -> IO Bool
handleOpponentMoves appData stateRef = do
  appState <- readIORef stateRef
  when (sideToPlay appState /= playerColor appData) $ do
    opponentHasPlayed <- not <$> isEmptyChan (opponentMovesChan appData)
    when opponentHasPlayed $ do
      opponentMove <- parseMove <$> readChan (opponentMovesChan appData)
      modifyIORef stateRef (\s -> s { position = applyMove opponentMove (sideToPlay s) (position s),
                                      sideToPlay = playerColor appData })
  requestRedraw appData
  return True


-- Drawing --
-------------

updateCanvas :: DrawingArea -> AppData -> IORef AppState -> IO ()
updateCanvas canvas appData stateRef = do
  (w, h) <- widgetGetSize canvas
  dw <- widgetGetDrawWindow canvas
  state <- readIORef stateRef
  renderWithDrawable dw (drawBoard w h appData state)

darkTileColor = (110, 128, 158)
lightTileColor = (209, 205, 184)

drawBoard :: Int -> Int -> AppData -> AppState -> Render ()
drawBoard w h appData appState = do
  let c = fromIntegral $ (min w h) - 2
  translate 1 1

  -- dark color background (dark squares)
  let (r,g,b) = darkTileColor in setSourceRGB (r/255) (g/255) (b/255)
  rectangle 0 0 c c
  fill
  
  -- light squares
  let (r,g,b) = lightTileColor in setSourceRGB (r/255) (g/255) (b/255)
  let lightSquares = map snd . filter fst
                     . zip (cycle (take 8 (cycle [True, False]) ++ take 8 (cycle [False, True])))
                     $ [ (x/8,y/8) | x <- [0..7], y <- [0..7] ]
  forM_ lightSquares $ \(x, y) -> rectangle (x*c) (y*c) (c/8) (c/8)
  fill

  -- black lines = grid
  setSourceRGB 0 0 0
  setLineWidth 2
  forM_ [0..8] $ \i -> do
    moveTo 0 (i*c/8) >> lineTo c (i*c/8)
    moveTo (i*c/8) 0 >> lineTo (i*c/8) c
  stroke

  -- highlight selected square (green)
  case selectedSquare appState of
    Nothing -> return ()
    Just (col, row) -> do
      setSourceRGB 0 255 0
      setLineWidth 4
      rectangle ((fromIntegral col - 1) * c / 8) ((8 - fromIntegral row) * c / 8) (c/8) (c/8)
      stroke

  -- draw the pieces themselves
  drawPosition c (piecesImagesMap appData) (position appState)

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
