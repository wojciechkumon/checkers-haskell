module Parser where
import Strings
import DataTypes
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

parsePos :: Parser Int
parsePos = do
             x <- int
             if (x<1 || x>32) then
               unexpected only1To32String
             else return x

parseMove = do
              x1 <- parsePos
              (char '-')
              x2 <- parsePos
              eof
              return $ MovePDN (x1,x2)

parseKill = do
              x1 <- sepBy (parsePos) (char 'x')
              eof
              if (length x1) > 1 then
                return $ KillPDN x1
              else unexpected startAndEndOfMinimum

parsePDN = try parseMove <|> parseKill
