module Board where
import Utils
import Data.Char
import Data.Maybe

data Piece = Piece {pieceType :: PieceType, pieceColor :: PieceColor} deriving Eq
data PieceType = Man | King deriving Eq
data PieceColor = White | Black deriving Eq

type Field = Maybe Piece
type Board = [[Field]]
type Position = (Int, Int)


-- PRINT --

instance Show Piece where
  show (Piece Man White) = "w"
  show (Piece Man Black) = "b"
  show (Piece King White) = "W"
  show (Piece King Black) = "B"

showField :: Field -> String
showField Nothing = "."
showField (Just a) = (show a)

showBoard :: Board -> String
showBoard = unlines . map (concatMap showField)

showBoardIndent :: Int -> Board -> String
showBoardIndent x = ('\n':) . concatMap ((('\n':take x (repeat ' ')) ++ ) . concatMap showField)


-- READ --

-- format: "w" "W" "b" "B" for white, white king, black, black king
instance Read Piece where
  readsPrec _ value = 
    tryParse [("w", (Piece Man White)), ("b", (Piece Man Black)), 
      ("W", (Piece King White)), ("B", (Piece King Black))]
    where tryParse [] = []
          tryParse ((attempt, result):xs) = 
            if (take (length attempt) value) == attempt then 
              [(result, drop (length attempt) value)]
            else tryParse xs

-- format: Read Piece format or '.' for empty
readField :: Char -> Field
readField '.' = Nothing
readField inputChar = Just ((read [inputChar])::Piece)

-- format: ".b.b.b.b\nb.b.b.b.\n........\nw.w.w.w.\n.w.w.w."
readBoard :: String -> Board
readBoard inputString =  map (map readField) (lines inputString)

-- format: "a1" - "h8"
readPosition :: String -> Position
readPosition [x, y] = (7 - ((ord y) - (ord '1')), (ord x) - (ord 'a'))

-- FUNCTIONS -- 

getOppositeColor :: PieceColor -> PieceColor
getOppositeColor White = Black
getOppositeColor Black = White

getField :: Board -> Position -> Field
getField board (a, b) = board!!a!!b

emptyField :: Field
emptyField = Nothing

isEmptyField :: Board -> Position -> Bool
isEmptyField board position = Nothing == getField board position

updateBoard :: Board -> Field -> Position -> Board
updateBoard = updateMatrix

deletePiece :: Board -> Position -> Board
deletePiece board position = updateBoard board emptyField position

movePiece :: Position -> Position -> Board -> Board
movePiece pos1 pos2 board = updateBoard (deletePiece board pos1) (getField board pos1) pos2

move :: String -> String -> Board -> Board
move pos1 pos2 = movePiece (readPosition pos1) (readPosition pos2)

isPositionOutside :: Position -> Bool
isPositionOutside (a, b) = a < 0 || b < 0 || a > 7 || b > 7

isPositionInside :: Position -> Bool
isPositionInside position = not (isPositionOutside position)

getColorPositions :: PieceColor -> Board -> [Position]
getColorPositions color board = [(a, b)|a<-[0..7],b<-[0..7], hasFieldColor color (getField board (a,b))]

hasFieldColor :: PieceColor -> Field -> Bool
hasFieldColor _ Nothing = False
hasFieldColor colorToCheck (Just (Piece _ pieceColor)) = colorToCheck == pieceColor