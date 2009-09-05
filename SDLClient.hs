import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Foldable as F

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG

import ChessTypes
import Common

squareSidePx = 96
boardSidePx = squareSidePx * 8

data AppData = AppData { screen :: SDL.Surface, piecesImagesMap :: Map Piece SDL.Surface }

main = SDL.withInit [SDL.InitVideo] $ do
  screen <- SDL.setVideoMode boardSidePx boardSidePx 0 [SDL.HWSurface, SDL.DoubleBuf]
  piecesImagesMap <- loadPiecesImages
  runReaderT mainLoop (AppData screen piecesImagesMap)
  
mainLoop = do
  events <- liftIO pollEvents
  unless (SDL.Quit `elem` events) $ do
    drawBoard
    drawPosition startingPosition
    (liftIO . SDL.flip) =<< asks screen
    mainLoop

drawBoard = sequence_ [drawSquare x y | x <- [0..7], y <- [0..7]]
drawSquare x y = do
  screen <- asks screen
  liftIO $ do
    let toPixel = SDL.mapRGB (SDL.surfaceGetPixelFormat screen)
    color <- if (x + y) `mod` 2 == 0
             then toPixel 255 255 255
             else toPixel 128 128 255
    SDL.fillRect screen
                 (Just $ SDL.Rect (x*squareSidePx) (y*squareSidePx) squareSidePx squareSidePx) 
                 color

drawPosition (Position piecesMap _) = drawPieces piecesMap
drawPieces = F.sequence_ . M.mapWithKey drawPiece
drawPiece (col, row) piece = do
  img <- (M.! piece) <$> asks piecesImagesMap
  screen <- asks screen
  liftIO $ SDL.blitSurface img Nothing screen (Just $ SDL.Rect x y 0 0)
    where x = (col - 1) * squareSidePx
          y = (8 - row) * squareSidePx
     

loadPiecesImages = M.fromList <$> mapM loadPieceImage pieces
  where pieceTypes = [King, Queen, Rook, Bishop, Knight, Pawn]
        pieces = [Piece pieceType color | color <- [Black, White], pieceType <- pieceTypes]
        loadPieceImage piece@(Piece pieceType color) =
            ((,) piece) <$> (IMG.load $ "img/" ++
                                        map toLower (show color) ++ "." ++
                                        map toLower (show pieceType) ++ ".png")

pollEvents = SDL.pollEvent >>= recur
    where recur SDL.NoEvent = return []
          recur evt         = (evt:) <$> pollEvents
