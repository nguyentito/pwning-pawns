module Common (startingPosition,
               applyMove,
               legalMoves,
               legalMovesMap)
where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

import ChessTypes

--
-- startingPosition section
--

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

--
-- applyMove section
--

applyMove :: Move -> Color -> Position -> Position

applyMove (Castling side) color (Position oldBoard oldState) = 
    Position newBoard newState
    where newState = oldState {castlingMap=newCastlingMap}
          newCastlingMap = M.fromList [((color, side), False) | side <- [Kingside, Queenside]]
                           `M.union` castlingMap oldState
          newBoard = M.insert (castlingRookDestCol side, row) (Piece Rook color) .
                     M.insert (castlingKingDestCol side, row) (Piece King color) .
                     M.delete (castlingRookOrigCol side, row) .
                     M.delete (castlingKingOrigCol     , row) $
                     oldBoard
          row = castlingRow color

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

--
-- legalMoves section
--

legalMovesMap :: Piece -> Square -> Position -> Map Square Move
legalMovesMap piece@(Piece _ color) square position = M.fromList assocList
    where assocList = map f moves
          f = (,) <$> associatedSquare color <*> id
          moves = legalMoves piece square position

associatedSquare :: Color -> Move -> Square -- depends on color just b/c of castling
associatedSquare color (StandardMove {moveDest=dest}) = dest
associatedSquare color (Castling side) = (castlingKingDestCol side, castlingRow color)


legalMoves :: Piece -> Square -> Position -> [Move] -- not a MoveFunction
legalMoves piece@(Piece pieceType _) = legalMoves' pieceType piece

type MoveFunction = Piece -> Square -> Position -> [Move]

legalMoves' :: PieceType -> MoveFunction

legalMoves' Pawn = pawnMoves
    
legalMoves' King = simpleMovement omniDir `concatMoves` castlingPossibilities

legalMoves' Knight  = simpleMovement $ [(,), flip (,)] <*> pm1 <*> pm2
    where pm2 = [(+) 2, flip (-) 2]

legalMoves' Queen = linearMovement omniDir
legalMoves' Rook = linearMovement orthogonalDir
legalMoves' Bishop = linearMovement diagonalDir


type MoveSquareDiff = (Int -> Int, Int -> Int)

orthogonalDir, diagonalDir, omniDir :: [MoveSquareDiff]
orthogonalDir = [(,), flip (,)] <*> pure id <*> pm1
diagonalDir = (,) <$> pm1 <*> pm1
omniDir = orthogonalDir ++ diagonalDir
pm1 = [(+) 1, flip (-) 1]


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


castlingPossibilities :: MoveFunction
castlingPossibilities (Piece _ color) _ (Position _ state) =
    map Castling . filter castlingPossible $ [Kingside, Queenside]
        where castlingPossible side = M.lookup (color, side) (castlingMap state)
                                      == Just True

pawnMoves :: MoveFunction
pawnMoves (Piece pieceType color) orig@(origCol, origRow) (Position board _) =
    -- will add en passant later
    map (makeStandardMove pieceType orig) . catMaybes $
    maybeDoubleAdvanceMvt : maybeAdvanceMvt : captureMvts
        where vDiff = case color of White -> ((+) 1);
                                    Black -> flip (-) 1
              maybeAdvanceMvt = maybeNonCaptureMvt (vDiff, id)
              maybeDoubleAdvanceMvt
                  | origRow == pawnRow = maybeNonCaptureMvt (vDiff . vDiff, id)
                  | otherwise = Nothing
              maybeNonCaptureMvt diff | destIsEmpty && (not outOfBounds) = Just dest
                                      | otherwise = Nothing
                  where dest@(destCol, destRow) = applyDiff diff orig
                        destIsEmpty = dest `M.notMember` board
                        outOfBounds = destCol < 1 || destCol > 8 ||
                                      destRow < 1 || destRow > 8
              captureMvts = filter hasEnemy . map (flip applyDiff orig) $
                            ((,) vDiff) <$> pm1
                  where hasEnemy square = case M.lookup square board of
                                            Just (Piece _ occupyingPieceColor)
                                                | occupyingPieceColor /= color -> True
                                            _ -> False
                        

applyDiff :: MoveSquareDiff -> Square -> Square
applyDiff (vDiff, hDiff) (origCol, origRow) = (vDiff origCol, hDiff origRow)

concatMoves :: MoveFunction -> MoveFunction -> MoveFunction
concatMoves f g = \a b c -> f a b c ++ g a b c

canLandOn movingPieceColor board square@(row, col)
    | row < 1 || row > 8 || col < 1 || col > 8 = False
    | otherwise = case M.lookup square board of
                    Just (Piece _ occupyingPieceColor)
                        | occupyingPieceColor == movingPieceColor -> False
                    _ -> True

--
-- Castling rules section
--

castlingRow White = 1
castlingRow Black = 8

castlingKingOrigCol = 5

castlingRookOrigCol Kingside = 8
castlingRookOrigCol Queenside = 1

castlingKingDestCol Kingside = 7
castlingKingDestCol Queenside = 3

castlingRookDestCol Kingside = 6
castlingRookDestCol Queenside = 4
