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
     <|> p_standardMove

p_queensideCastling = QueensideCastling <$ string "0-0-0"
p_kingsideCastling = KingsideCastling <$ string "0-0"

p_standardMove = StandardMove <$> p_pieceType <*> p_square <*> (p_separator *> p_square)

p_pieceType = choice (map (\(piece, letter) -> piece <$ char letter) pieceLetterAssocList)
            <|> return Pawn
       
p_square = (,) <$> p_col <*> p_row
p_col = (+1) . (flip (-) (ord 'a')) . ord <$> oneOf ['a'..'h']
p_row = digitToInt <$> oneOf ['1'..'8']

p_separator = char '-' <|> char 'x'

printMove KingsideCastling = "0-0"
printMove QueensideCastling = "0-0-0"
printMove (StandardMove pieceType (origCol, origRow) (destCol, destRow)) =
    maybe const (:) (lookup pieceType pieceLetterAssocList) $
    [ colToChar origCol, rowToChar origRow, '-',
      colToChar destCol, rowToChar destCol ]
        where colToChar = chr . (+ (ord a)) . (flip (-) 1)
              rowToChar = intToDigit
