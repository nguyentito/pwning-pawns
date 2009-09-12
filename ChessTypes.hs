module ChessTypes where

import Data.Map (Map)

type Square = (Int, Int)

data Piece = Piece PieceType Color
             deriving (Eq, Ord)
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
               deriving (Eq, Show, Ord)
data Color = Black | White
           deriving (Eq, Show, Ord)

data Position = Position Board GameState
type Board = Map Square Piece
data GameState = GameState {
      castlingMap :: Map (Color, CastlingSide) Bool
    }

boardToPosition = flip Position (GameState (M.fromList [((White, Kingside), False)
                                                        ((White, Queenside), False),
                                                        ((Black, Kingside), False),
                                                        ((Black, Queenside), False)]

data CastlingSide = Queenside | Kingside

data Move = Castling CastlingSide
          | EnPassant { moveOrig :: Square, targetCol :: Int }
          | StandardMove { movePiece :: PieceType,
                           moveOrig :: Square,
                           moveDest :: Square }
            deriving (Show)