module AlgebraicNotation (parseMove, printMove) where

import Control.Applicative
import Data.Char
import Data.List
import Text.Parsec hiding ((<|>))
import Text.Parsec.String

import ChessTypes

pieceLetterAssocList = zip [King, Queen, Rook, Bishop, Knight] "KQRBN"

parseMove str = case parse p_move "" str of
                  Left err -> error $ show err
                  Right result -> result

p_move :: Parser Move
p_move = try p_queensideCastling
     <|> try p_kingsideCastling
     <|> try p_enPassant
     <|> p_standardMove

p_queensideCastling = (Castling Queenside) <$ string "0-0-0"
p_kingsideCastling = (Castling Kingside) <$ string "0-0"

p_standardMove = StandardMove <$> p_pieceType <*> p_square <*> (p_separator *> p_square)

p_enPassant = EnPassant <$> p_square <*> (p_separator *> p_square) <* string " e.p."

p_pieceType = choice (map (\(piece, letter) -> piece <$ char letter) pieceLetterAssocList)
            <|> return Pawn
       
p_square = (,) <$> p_col <*> p_row
p_col = (+1) . (flip (-) (ord 'a')) . ord <$> oneOf ['a'..'h']
p_row = digitToInt <$> oneOf ['1'..'8']

p_separator = char '-' <|> char 'x'

printMove (Castling Kingside) = "0-0"
printMove (Castling Queenside) = "0-0-0"
printMove (StandardMove pieceType (origCol, origRow) (destCol, destRow)) =
    maybe id (:) (lookup pieceType pieceLetterAssocList) $
    [ colToChar origCol, rowToChar origRow, '-', colToChar destCol, rowToChar destRow ]
printMove (EnPassant (origCol, origRow) (destCol, destRow)) =
    [ colToChar origCol, rowToChar origRow, 'x', colToChar destCol, rowToChar destRow ]
    ++ " e.p."

colToChar = chr . (+ (ord 'a')) . (flip (-) 1)
rowToChar = intToDigit
