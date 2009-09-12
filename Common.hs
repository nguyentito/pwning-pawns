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

applyMove (Castling side) color (Position oldBoard oldState) = 
    Position newBoard newState
    where newState = oldState {castlingMap=newCastlingMap}
          newCastlingMap = M.fromList [((color, Kingside), False), ((color, Queenside), True)]
                           `M.union` castlingMap oldState
          newBoard = M.insert (newRookCol, row) (Piece Rook color) .
                     M.insert (newKingCol, row) (Piece King color) .
                     M.delete (oldRookCol, row) .
                     M.delete (oldKingCol, row) $
                     oldBoard
          row = case color of
                  White -> 1
                  Black -> 8
          oldKingCol = 5
          (oldRookCol) = case side of
                           Kingside -> 8
                           Queenside -> 1
          (newKingCol, newRookCol) = case side of
                                       Kingside -> (7, 6)
                                       Queenside -> (3, 4)

applyMove (StandardMove pieceType orig dest) color (Position board _) =
    boardToPosition $ M.insert dest (Piece pieceType color) . M.delete orig $ board

applyMove (EnPassant _ _) _ _ = undefined

legalMoves :: Piece -> Square -> Position -> [Move]

legalMoves (Piece Pawn _) orig@(origCol, origRow) _ =
    [StandardMove { moveOrig=orig, moveDest=(origCol, origRow+1), movePiece=Pawn }]

legalMoves _ _ _ = []
