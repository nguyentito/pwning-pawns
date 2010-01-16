module ChessTypes where

import Data.Map (Map)
import qualified Data.Map as M (fromList)

type Square = (Int, Int)

data Piece = Piece PieceType Color
             deriving (Eq, Ord)
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
               deriving (Eq, Show, Ord, Read)
data Color = Black | White
           deriving (Eq, Show, Ord, Read)

otherColor White = Black
otherColor Black = White

data Position = Position Board GameState
type Board = Map Square Piece
data GameState = GameState {
      castlingMap :: Map (Color, CastlingSide) Bool,
      enPassantPossibility :: Maybe Square
    }

boardToPosition = flip Position (GameState { castlingMap = defaultCastlingMap,
                                             enPassantPossibility = Nothing })
    where defaultCastlingMap = M.fromList [((White, Kingside), True),
                                           ((White, Queenside), True),
                                           ((Black, Kingside), True),
                                           ((Black, Queenside), True)]

data CastlingSide = Queenside | Kingside
                    deriving (Eq, Show, Ord)

data Move = Castling CastlingSide
          | EnPassant { moveOrig :: Square, moveDest :: Square }
          | StandardMove { movePiece :: PieceType,
                           moveOrig :: Square,
                           moveDest :: Square }
            deriving (Show)

makeStandardMove = StandardMove
