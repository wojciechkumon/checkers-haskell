module Board where
import Utils
import DataTypes


-- PRINT --

showField :: Field -> String
showField Nothing = "."
showField (Just a) = (show a)

showBoard :: Board -> String
showBoard = unlines . map (concatMap showField)

showBoardIndent :: Int -> Board -> String
showBoardIndent x = ('\n':) . concatMap ((('\n':take x (repeat ' ')) ++ ) . concatMap showField)


-- READ --

-- format: Read Piece format or '.' for empty
readField :: Char -> Field
readField '.' = Nothing
readField inputChar = Just ((read [inputChar])::Piece)

-- format: ".b.b.b.b\nb.b.b.b.\n........\nw.w.w.w.\n.w.w.w."
readBoard :: String -> Board
readBoard inputString =  map (map readField) (lines inputString)

-- format: "1" - "32"
readPosition :: Int -> Position
readPosition val = ((val - 1) `quot` 4, if (((val-1)`quot`4) `mod` 2 == 0) then 1+2*((val-1) `mod` 4) else 2*((val-1) `mod` 4))

-- FUNCTIONS -- 

getOppositeColor :: PieceColor -> PieceColor
getOppositeColor White = Black
getOppositeColor Black = White

getField :: Board -> Position -> Field
getField board (a, b) = board!!a!!b

updateBoard :: Board -> Field -> Position -> Board
updateBoard = updateMatrix

deletePiece :: Board -> Position -> Board
deletePiece board position = updateBoard board Nothing position

movePiece :: Position -> Position -> Board -> Board
movePiece pos1 pos2 board = updateBoard (deletePiece board pos1) (getField board pos1) pos2

move :: Int -> Int -> Board -> Board
move pos1 pos2 = movePiece (readPosition pos1) (readPosition pos2)

isPositionOutside :: Position -> Bool
isPositionOutside (a, b) = a < 0 || b < 0 || a > 7 || b > 7

isPositionInside :: Position -> Bool
isPositionInside position = not (isPositionOutside position)

getColorPositions :: PieceColor -> Board -> [Position]
getColorPositions color board = [(a, b) | a <- [0..7], b <- [0..7], hasFieldColor color (getField board (a,b))]

hasFieldColor :: PieceColor -> Field -> Bool
hasFieldColor _ Nothing = False
hasFieldColor colorToCheck (Just (Piece _ pieceColor)) = colorToCheck == pieceColor
