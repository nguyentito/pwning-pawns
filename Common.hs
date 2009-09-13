module Common where

import Control.Applicative
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
          newCastlingMap = M.fromList [((color, side), False) | side <- [Kingside, Queenside]]
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

applyMove (StandardMove pieceType orig dest) color (Position oldBoard oldState) =
    makePositionWithNewState $
    M.insert dest (Piece pieceType color) .
    M.delete orig $ oldBoard
        where makePositionWithNewState = flip Position newState
              newState = oldState { castlingMap = modifyCastlingMap (castlingMap oldState) }
              modifyCastlingMap = case pieceType of
                                    King -> M.insert (color, Kingside) False .
                                            M.insert (color, Queenside) False
                                    Rook -> if (orig == (1, castlingRow))
                                            then M.insert (color, Queenside) False
                                            else if (orig == (8, castlingRow))
                                                 then M.insert (color, Kingside) False
                                                 else id
                                    _ -> id
              castlingRow = case color of
                              White -> 1
                              Black -> 8

applyMove (EnPassant _ _) _ _ = undefined

legalMoves :: Piece -> Square -> Position -> [Move]
legalMoves piece@(Piece pieceType _) = legalMoves' pieceType piece

type MoveFunction = Piece -> Square -> Position -> [Move]

legalMoves' :: PieceType -> MoveFunction

legalMoves' Pawn = pawnMoves
    
legalMoves' King = simpleMovement omniDir `concatMoves` castlingPossibilities

legalMoves' Knight  = simpleMovement $ ((,) <$> pm1 <*> pm2) ++
                                       ((,) <$> pm2 <*> pm1)
    where pm2 = [(+) 2, flip (-) 2]

legalMoves' Queen = linearMovement omniDir
legalMoves' Rook = linearMovement orthogonalDir
legalMoves' Bishop = linearMovement diagonalDir

orthogonalDir, diagonalDir, omniDir :: [MoveSquareDiff]
orthogonalDir = map ((,) id) pm1 ++ map (flip (,) id) pm1
diagonalDir = (,) <$> pm1 <*> pm1
omniDir = orthogonalDir ++ diagonalDir
pm1 = [(+) 1, flip (-) 1]

type MoveSquareDiff = (Int -> Int, Int -> Int)

simpleMovement :: [MoveSquareDiff] -> MoveFunction
simpleMovement mvts (Piece pieceType color) orig (Position board _) =
    map (makeStandardMove pieceType orig) .
    filter (canLandOn color board) .
    map diffToDest $ mvts
        where diffToDest = flip applyDiff orig
    
linearMovement :: [MoveSquareDiff] -> MoveFunction
linearMovement mvts (Piece pieceType color) orig (Position board _) =
    map (makeStandardMove pieceType orig) . concatMap f $ mvts
        where f mvtDiff = takeWhile (canLandOn color board) .
                          iterate (applyDiff mvtDiff) $
                          orig

concatMoves :: MoveFunction -> MoveFunction -> MoveFunction
concatMoves f g = \a b c -> f a b c ++ g a b c

castlingPossibilities :: MoveFunction
castlingPossibilities (Piece _ color) _ (Position _ state) =
    map Castling . filter castlingPossible $ [Kingside, Queenside]
        where castlingPossible side = M.lookup (color, side) (castlingMap state)
                                      == Just True

pawnMoves :: MoveFunction
pawnMoves = undefined

applyDiff :: MoveSquareDiff -> Square -> Square
applyDiff (vDiff, hDiff) (origCol, origRow) = (vDiff origCol, hDiff origRow)

canLandOn movingPieceColor board square@(row, col)
    | row < 1 || row > 8 || col < 1 || col > 8 = False
    | otherwise = case M.lookup square board of
                    Just (Piece _ occupyingPieceColor)
                        | occupyingPieceColor == movingPieceColor -> False
                    _ -> True
