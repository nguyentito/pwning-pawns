module ChessTypes where

import Data.Map (Map)

type Square = (Int, Int)

data Piece = Piece PieceType Color
             deriving (Eq)
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
               deriving (Eq, Show)
data Color = Black | White
           deriving (Eq, Show)

data Position = Position Board GameState
type Board = Map Square Piece
data GameState = GameState () -- may be used for castling and/or en passant

boardToPosition = flip Position (GameState ())

data Move = StandardMove { movePiece :: PieceType,
                           moveOrig :: Square,
                           moveDest :: Square
                         }
          | KingsideCastling
          | QueensideCastling
