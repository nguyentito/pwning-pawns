module ChessTypes where

import Data.Map (Map)

type Square = (Int, Int)

data Piece = Piece PieceType Color
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
data Color = Black | White

data Position = Position Board GameState
type Board = Map Square Piece
data GameState = GameState () -- may be used for castling and/or en passant

data Move = Move {
      movePiece :: PieceType,
      moveOrig :: Square,
      moveDest :: Square
    }
