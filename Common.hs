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
              newState = oldState { castlingMap = modifyCastlingMap (castlingMap oldState),
                                    enPassantPossibility = newEnPassantPossibility
                                  }
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
              newEnPassantPossibility
                  | moveIsDoublePawnAdvance = Just (fst orig, (snd orig + snd dest) `div` 2)
                  | otherwise               = Nothing
              moveIsDoublePawnAdvance = pieceType == Pawn
                                        && fst orig == fst dest
                                        && abs (snd orig - snd dest) == 2

applyMove (EnPassant orig dest) color (Position oldBoard oldState) =
    Position newBoard newState
        where newBoard = M.insert dest (Piece Pawn color) .
                         M.delete orig .
                         M.delete capturedPawnLocation $
                         oldBoard
              newState = oldState { enPassantPossibility = Nothing }
              capturedPawnLocation = (fst dest, snd orig)

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
associatedSquare color (EnPassant {moveDest=dest}) = dest
associatedSquare color (Castling side) = (castlingKingDestCol side, castlingRow color)


legalMoves :: Piece -> Square -> Position -> [Move]
legalMoves piece@(Piece _ color) square position = filter (not . leavesKingInCheck) moves
    where moves = legalMoves' piece square position
          leavesKingInCheck move = sideIsInCheck color (applyMove move color position)

sideIsInCheck :: Color -> Position -> Bool
sideIsInCheck color position@(Position board _) = any threatensKing opponentMoves
    where opponentMoves = concatMap (\(square, piece) -> legalMoves' piece square position) .
                          filter ((/= color) . pieceColor . snd) $ M.toList board
          pieceColor (Piece _ c) = c
          threatensKing (StandardMove { moveDest = dest }) =
              (Just dest) == maybeKingSquare
          threatensKing _ = False
          maybeKingSquare = (fst <$>) . find (((Piece King color) ==) . snd) $ M.toList board

-- This version doesn't check if your king is in check after a move
-- (in this case, the move should be illegal)
legalMoves' :: Piece -> Square -> Position -> [Move]
legalMoves' piece@(Piece pieceType _) = f pieceType piece
    where f Pawn = pawnMoves
          f King = simpleMovement omniDir `concatMoves` castlingPossibilities
          f Knight = simpleMovement $ [(,), flip (,)] <*> pm1 <*> pm2
              where pm2 = [(+) 2, flip (-) 2]
          f Queen = linearMovement omniDir
          f Rook = linearMovement orthogonalDir
          f Bishop = linearMovement diagonalDir

      
type MoveSquareDiff = (Int -> Int, Int -> Int)

orthogonalDir, diagonalDir, omniDir :: [MoveSquareDiff]
orthogonalDir = [(,), flip (,)] <*> pure id <*> pm1
diagonalDir = (,) <$> pm1 <*> pm1
omniDir = orthogonalDir ++ diagonalDir
pm1 = [(+) 1, flip (-) 1]


type MoveFunction = Piece -> Square -> Position -> [Move]

simpleMovement :: [MoveSquareDiff] -> MoveFunction
simpleMovement mvts (Piece pieceType color) orig (Position board _) =
    map (makeStandardMove pieceType orig) .
    filter (canLandOn color board) .
    map diffToDest $ mvts
        where diffToDest = flip applyDiff orig                           
    
linearMovement :: [MoveSquareDiff] -> MoveFunction
linearMovement mvts (Piece pieceType color) orig (Position board _) =
    map (makeStandardMove pieceType orig) . concatMap f $ mvts
        where f mvtDiff = takeUntilObstacle color board .
                          tail . iterate (applyDiff mvtDiff) $
                          orig

canLandOn movingPieceColor board square@(row, col)
    | row < 1 || row > 8 || col < 1 || col > 8 = False
    | otherwise = case M.lookup square board of
                    Just (Piece _ occupyingPieceColor)
                        | occupyingPieceColor == movingPieceColor -> False
                    _ -> True

takeUntilObstacle movingPieceColor board (square:otherSquares)
    | not $ canLandOn movingPieceColor board square = []
    | M.member square board = [square]
    | otherwise = square : takeUntilObstacle movingPieceColor board otherSquares


castlingPossibilities :: MoveFunction
castlingPossibilities (Piece _ color) _ (Position board state) =
    map Castling . filter castlingPossible $ [Kingside, Queenside]
        where castlingPossible side =
                  (M.lookup (color, side) (castlingMap state) == Just True)
                  && all (`M.notMember` board) [(col, castlingRow color)
                                                | col <- castlingColsInBetween side]

pawnMoves :: MoveFunction
pawnMoves (Piece pieceType color) orig@(origCol, origRow) (Position board state) =
    maybeAddEnPassant .
    map (makeStandardMove pieceType orig) . catMaybes $
    maybeDoubleAdvanceMvt : maybeAdvanceMvt : map Just captureMvts
        where vDiff = case color of White -> ((+) 1);
                                    Black -> flip (-) 1
              maybeAdvanceMvt = maybeNonCaptureMvt (id, vDiff)
              maybeDoubleAdvanceMvt
                  | origRow == pawnRow && isJust maybeAdvanceMvt =
                      maybeNonCaptureMvt (id, vDiff . vDiff)
                  | otherwise = Nothing
              maybeNonCaptureMvt diff | destIsEmpty && (not outOfBounds) = Just dest
                                      | otherwise = Nothing
                  where dest@(destCol, destRow) = applyDiff diff orig
                        destIsEmpty = dest `M.notMember` board
                        outOfBounds = destCol < 1 || destCol > 8 ||
                                      destRow < 1 || destRow > 8
              captureMvts = filter hasEnemy . map (flip applyDiff orig) $
                            (flip (,) vDiff) <$> pm1
                  where hasEnemy square = case M.lookup square board of
                                            Just (Piece _ occupyingPieceColor)
                                                | occupyingPieceColor /= color -> True
                                            _ -> False
              pawnRow = case color of White -> 2;
                                      Black -> 7
              maybeAddEnPassant = case enPassantPossibility state of
                                    Just dest@(destCol, destRow)
                                        | abs (origCol - destCol) == 1 && destRow == vDiff origRow ->
                                            ( (EnPassant orig dest) :)
                                    _ -> id
                        

applyDiff :: MoveSquareDiff -> Square -> Square
applyDiff (vDiff, hDiff) (origCol, origRow) = (vDiff origCol, hDiff origRow)

concatMoves :: MoveFunction -> MoveFunction -> MoveFunction
concatMoves f g = \a b c -> f a b c ++ g a b c


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

castlingColsInBetween Kingside = [6, 7]
castlingColsInBetween Queenside = [2, 3, 4]
