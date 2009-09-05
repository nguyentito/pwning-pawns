import Control.Applicative
import Control.Monad

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG

squareSidePx = 72
boardSidePx = squareSidePx * 8

main = SDL.withInit [SDL.InitVideo] $ do
  screen <- SDL.setVideoMode boardSidePx boardSidePx 0 [SDL.HWSurface, SDL.DoubleBuf]
  mainLoop screen
  
mainLoop screen = do
  events <- pollEvents
  unless (SDL.Quit `elem` events) $ do
    drawBoard screen
    SDL.flip screen
    mainLoop screen

drawBoard screen = mapM_ (drawSquare screen) [(x, y) | x <- [0..7], y <- [0..7]]

drawSquare screen (x, y) = do
  let toPixel = SDL.mapRGB (SDL.surfaceGetPixelFormat screen)
  color <- if (x + y) `mod` 2 == 0
           then toPixel 255 255 255
           else toPixel 128 128 255
  SDL.fillRect screen
               (Just $ SDL.Rect (x*squareSidePx) (y*squareSidePx) squareSidePx squareSidePx) 
               color

pollEvents = SDL.pollEvent >>= recur
    where recur SDL.NoEvent = return []
          recur evt         = (evt:) <$> pollEvents
