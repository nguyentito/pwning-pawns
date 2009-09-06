module Common where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M

import ChessTypes

startingPosition :: Position
startingPosition = boardToPosition . M.fromList $
                   majorPiecesRow White 1 ++
                   pawnRow White 2 ++
                   pawnRow Black 7 ++
                   majorPiecesRow Black 8
    where majorPiecesRow = piecesRow [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
          pawnRow = piecesRow (replicate 8 Pawn)
          piecesRow pieceTypeList color rowNumber = 
              zipWith f [1..] pieceTypeList
                  where f colNumber pieceType = ((colNumber, rowNumber), Piece pieceType color)

applyMove :: Move -> Color -> Position -> Position
applyMove KingsideCastling _ _ = undefined
applyMove QueensideCastling _ _ = undefined
applyMove (StandardMove pieceType orig dest) color (Position board _) =
    boardToPosition $ M.insert dest (Piece pieceType color) . M.delete orig $ board

legalMoves :: Piece -> Square -> Position -> [Move]
legalMoves (Piece Pawn _) orig@(origCol, origRow) _ =
    [StandardMove { moveOrig=orig, moveDest=(origCol, origRow+1), movePiece=Pawn }]

