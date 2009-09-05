module AlgebraicNotationParser (parseMove) where

import Control.Applicative
import Data.Char
import Data.List
import Text.Parsec hiding ((<|>))
import Text.Parsec.String

import ChessTypes

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

p_pieceType = choice (zipWith (<$) [King, Queen, Rook, Bishop, Knight]
                                   (map char "KQRBN"))
          <|> return Pawn
       
p_square = (,) <$> p_col <*> p_row
p_col = (+1) . (flip (-) (ord 'a')) . ord <$> oneOf ['a'..'h']
p_row = digitToInt <$> oneOf ['1'..'8']

p_separator = char '-' <|> char 'x'
