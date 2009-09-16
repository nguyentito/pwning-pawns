module SDLClient (mainSDL) where

import Control.Concurrent
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Foldable as F

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG

import AlgebraicNotation
import ChessTypes
import Common

squareSidePx = 96
boardSidePx = squareSidePx * 8

data AppData = AppData {
      screen :: SDL.Surface,
      piecesImagesMap :: Map Piece SDL.Surface,
      playerMovesChan :: Chan String,
      opponentMovesChan :: Chan String,
      playerColor :: Color
    }

data AppState = AppState {
      position :: Position,
      selectedSquare :: Maybe Square,
      currentMovesMap :: Maybe (Map Square Move),
      sideToPlay :: Color
    }

mainSDL :: Chan String -> Chan String -> Color -> IO ()
mainSDL playerMovesChan opponentMovesChan playerColor = do
  SDL.withInit [SDL.InitVideo] $ do
    screen <- SDL.setVideoMode boardSidePx boardSidePx 0 [SDL.HWSurface, SDL.DoubleBuf]
    piecesImagesMap <- loadPiecesImages
    let initData = AppData screen piecesImagesMap
                           playerMovesChan opponentMovesChan
                           playerColor
        initState = AppState startingPosition Nothing Nothing White
    runAppMonad mainLoop initData initState


type AppMonad a = ReaderT AppData (StateT AppState IO) a
               
runAppMonad :: AppMonad a -> AppData -> AppState -> IO a
runAppMonad appMonad initData initState =
    evalStateT (runReaderT appMonad initData) initState


mainLoop :: AppMonad ()
mainLoop = do
  events <- liftIO pollEvents
  unless (SDL.Quit `elem` events) $ do
    handleEvents events
    handleOpponentMoves
    drawBoard
    drawLayer2
    drawPosition
    (liftIO . SDL.flip) =<< asks screen
    mainLoop

handleEvents :: [SDL.Event] -> AppMonad ()
handleEvents = mapM_ f
    where f (SDL.MouseButtonUp x y SDL.ButtonLeft) = do
            onClickedSquare ((xToCol . fromIntegral $ x),
                             (yToRow . fromIntegral $ y))
          f _ = return ()


onClickedSquare square@(col, row) = do
  selectedSquare <- gets selectedSquare
  case selectedSquare of
    Just (selectedCol, selectedRow)
        | col == selectedCol && row == selectedRow -> selectSquare Nothing
        | otherwise -> do
            (Just movesMap) <- gets currentMovesMap
            case M.lookup square movesMap of
              Just move -> do
                applyPlayerMove move
                selectSquare Nothing
              Nothing -> return ()
    Nothing -> selectSquare $ Just square
  
selectSquare Nothing = modify (\st -> st { selectedSquare = Nothing,
                                           currentMovesMap = Nothing })
selectSquare maybeSquare@(Just square) = do
  position@(Position board _) <- gets position
  case M.lookup square board of
    Nothing -> return ()
    Just piece -> modify (\st -> st { selectedSquare = maybeSquare,
                                      currentMovesMap = Just $ legalMovesMap piece square position })

applyPlayerMove move = do 
  color <- asks playerColor
  modify (\st -> st { position = applyMove move color (position st),
                      sideToPlay = otherColor color })
  chan <- asks playerMovesChan
  liftIO $ writeChan chan (printMove move)


handleOpponentMoves = do
  playerColor <- asks playerColor
  sideToPlay <- gets sideToPlay
  when (sideToPlay /= playerColor) $ do
    opponentMovesChan <- asks opponentMovesChan
    opponentHasPlayed <- liftIO $ not <$> (isEmptyChan opponentMovesChan)
    when opponentHasPlayed $ do
      opponentMove <- liftIO $ parseMove <$> (readChan opponentMovesChan)
      modify (\st -> st { position = applyMove opponentMove sideToPlay (position st),
                          sideToPlay = playerColor })


drawBoard = sequence_ [drawSquare x y | x <- [0..7], y <- [0..7]]
drawSquare x y = do
  screen <- asks screen
  liftIO $ do
    let toPixel = SDL.mapRGB (SDL.surfaceGetPixelFormat screen)
    color <- if (x + y) `mod` 2 == 0
             then toPixel 255 255 255
             else toPixel 128 128 128
    SDL.fillRect screen
                 (Just $ SDL.Rect (x*squareSidePx) (y*squareSidePx) squareSidePx squareSidePx) 
                 color

drawLayer2 = do
  sq <- gets selectedSquare
  case sq of
    Just coords -> highlightSquare coords (0, 0, 255)
    Nothing -> return ()
  maybeMovesMap <- gets currentMovesMap
  case maybeMovesMap of
    Just movesMap -> mapM_ (flip highlightSquare (0, 255, 0)) (M.keys movesMap)
    Nothing -> return ()

highlightSquare (col, row) (r, g, b) = do
  screen <- asks screen
  liftIO $ do
    color <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) r g b
    let drawRect x y w h = () <$ SDL.fillRect screen (Just $ SDL.Rect x y w h) color
    drawRect x y 5 squareSidePx
    drawRect x y squareSidePx 5
    drawRect (x + squareSidePx - 5) y 5 squareSidePx
    drawRect x (y + squareSidePx - 5) squareSidePx 5
      where (x, y) = (colToX col, rowToY row)


drawPosition = (\(Position piecesMap _) -> drawPieces piecesMap) =<< gets position
drawPieces = F.sequence_ . M.mapWithKey drawPiece
drawPiece (col, row) piece = do
  img <- (M.! piece) <$> asks piecesImagesMap
  screen <- asks screen
  liftIO $ SDL.blitSurface img Nothing screen (Just $ SDL.Rect x y 0 0)
    where (x, y) = (colToX col, rowToY row)

loadPiecesImages = M.fromList <$> mapM loadPieceImage pieces
  where pieceTypes = [King, Queen, Rook, Bishop, Knight, Pawn]
        pieces = [Piece pieceType color | color <- [Black, White], pieceType <- pieceTypes]
        loadPieceImage piece@(Piece pieceType color) =
            ((,) piece) <$> (IMG.load $ "img/" ++
                                        map toLower (show color) ++ "." ++
                                        map toLower (show pieceType) ++ ".png")

pollEvents :: IO [SDL.Event]
pollEvents = SDL.pollEvent >>= recur
    where recur SDL.NoEvent = return []
          recur evt         = (evt:) <$> pollEvents

colToX col = (col - 1) * squareSidePx
xToCol x = (x `div` squareSidePx) + 1
rowToY row = (8 - row) * squareSidePx
yToRow y = 8 - (y `div` squareSidePx)
