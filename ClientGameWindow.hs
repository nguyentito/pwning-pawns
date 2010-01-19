module ClientGameWindow (startGameWindow)
where

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
import System.IO.Unsafe
import Text.Printf

import Graphics.UI.Gtk hiding (fill, Color)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Cairo
import Graphics.Rendering.Cairo

import AlgebraicNotation
import ChessTypes
import Common
import ClientThemeManager

-- Main --
----------

data AppData = AppData {
      playerMovesChan :: Chan String,
      opponentMovesChan :: Chan String,
      playerColor :: Color,
      requestRedraw :: IO (),
      setStatusMessage :: String -> IO ()
    }

data AppState = AppState {
      position :: Position,
      selectedSquare :: Maybe Square,
      currentMovesMap :: Maybe (Map Square Move),
      currentStatus :: GameStatus
    }

data GameStatus = GameOngoing { sideToPlay :: Color }
                | Checkmate { gameWinner :: Color }
                | Draw
                  deriving (Eq)

startGameWindow :: Chan String -> Chan String -> Color -> IO Window
startGameWindow playerMovesChan opponentMovesChan playerColor = do
  Just xml <- xmlNew "client-game-window.glade"
  window <- xmlGetWidget xml castToWindow "window"
  canvas <- xmlGetWidget xml castToDrawingArea "canvas"
  toolbar <- xmlGetWidget xml castToToolbar "toolbar"
  quitButton <- toolbarAppendNewButton toolbar "gtk-quit" Nothing
  statusLabel <- xmlGetWidget xml castToLabel "statusLabel"
  set statusLabel [ labelText := "C'est au tour des blancs de jouer." ]
  widgetShowAll window
  let appData = AppData playerMovesChan opponentMovesChan playerColor
                        (widgetQueueDraw canvas)
                        (\msg -> set statusLabel [ labelText := msg ])
  stateRef <- newIORef $ AppState {
                          position = startingPosition,
                          selectedSquare = Nothing,
                          currentMovesMap = Nothing,
                          currentStatus = GameOngoing { sideToPlay = White }
                        }
  onDelete window $ const (not <$> confirmQuitGame window)
  onClicked quitButton $ do
    userConfirmation <- confirmQuitGame window
    when userConfirmation $ widgetDestroy window
  onExpose canvas $ const (True <$ updateCanvas canvas appData stateRef)
  onButtonPress canvas $ handleButtonPress canvas appData stateRef
  timeoutAdd (handleOpponentMoves appData stateRef) 100
  return window

confirmQuitGame :: Window -> IO Bool
confirmQuitGame window = do
  dialog <- messageDialogNew (Just window) [DialogModal]
                             MessageQuestion ButtonsYesNo
                             "Voulez-vous vraiment quitter la partie ?"
  response <- dialogRun dialog
  widgetDestroy dialog
  return $ response == ResponseYes

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
  when (currentStatus appState == (GameOngoing { sideToPlay = playerColor appData })) $ do
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
  modifyIORef stateRef $ \s -> s { position = applyMove move (playerColor appData) (position s) }
  updateStatusAfterMove appData stateRef
  writeChan (playerMovesChan appData) $ printMove move

updateStatusAfterMove :: AppData -> IORef AppState -> IO ()
updateStatusAfterMove appData stateRef = do
  appState <- readIORef stateRef
  let setNewStatus msg newStatus = do
        modifyIORef stateRef $ \s -> s { currentStatus = newStatus }
        setStatusMessage appData msg
  case currentStatus appState of
    GameOngoing { sideToPlay = color } -> do
        let newSideToPlay = otherColor color
            sideName White = "blancs"
            sideName Black = "noirs"
        if (isCheckmated newSideToPlay (position appState))
          then setNewStatus (printf "Échec et mat ; les %s ont gagné." (sideName color))
               $ Checkmate { gameWinner = color }
          else setNewStatus (printf "C'est au tour des %s de jouer" (sideName newSideToPlay))
               $ GameOngoing { sideToPlay = newSideToPlay }
    _ -> return ()

-- Timer-triggered actions --
-----------------------------

handleOpponentMoves :: AppData -> IORef AppState -> IO Bool
handleOpponentMoves appData stateRef = do
  appState <- readIORef stateRef
  let opponentColor = otherColor (playerColor appData)
  when (currentStatus appState == GameOngoing { sideToPlay = opponentColor }) $ do
    opponentHasPlayed <- not <$> isEmptyChan (opponentMovesChan appData)
    when opponentHasPlayed $ do
      opponentMove <- parseMove <$> readChan (opponentMovesChan appData)
      modifyIORef stateRef (\s -> s { position = applyMove opponentMove opponentColor (position s) })
      updateStatusAfterMove appData stateRef
  requestRedraw appData
  return True


-- Drawing --
-------------

updateCanvas :: DrawingArea -> AppData -> IORef AppState -> IO ()
updateCanvas canvas appData stateRef = do
  (w, h) <- widgetGetSize canvas
  dw <- widgetGetDrawWindow canvas
  state <- readIORef stateRef
  withTheme $ \theme ->
      renderWithDrawable dw (drawBoard w h theme appData state)

drawBoard :: Int -> Int -> Theme -> AppData -> AppState -> Render ()
drawBoard w h theme appData appState = do
  let c = fromIntegral $ (min w h) - 2
  translate 1 1

  -- dark color background (dark squares)
  let (r,g,b) = darkTileColor theme in setSourceRGB (r/255) (g/255) (b/255)
  rectangle 0 0 c c
  fill
  
  -- light squares
  let (r,g,b) = lightTileColor theme in setSourceRGB (r/255) (g/255) (b/255)
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

  -- highlight possible moves (green)
  case currentMovesMap appState of
    Nothing -> return ()
    Just movesMap -> do
      setSourceRGB 0 255 0
      setLineWidth 4
      forM_ (M.keys movesMap) $ \(col, row) -> do
        rectangle ((fromIntegral col - 1) * c / 8) ((8 - fromIntegral row) * c / 8) (c/8) (c/8)
      stroke

  -- highlight selected square (blue)
  case selectedSquare appState of
    Nothing -> return ()
    Just (col, row) -> do
      setSourceRGB 0 0 255
      setLineWidth 4
      rectangle ((fromIntegral col - 1) * c / 8) ((8 - fromIntegral row) * c / 8) (c/8) (c/8)
      stroke

  -- draw the pieces themselves
  drawPosition c (piecesImagesMap theme) (position appState)

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
