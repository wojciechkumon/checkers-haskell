module Moves where
import Board
import Utils

-- PieceColor indicates whose turn it is
type State = (PieceColor, Board)

diagonal :: [Position]
diagonal = [(1,1),(-1,-1),(1,-1),(-1,1)]

forwardDiagonal :: [Position]
forwardDiagonal = [(-1,1), (-1, -1)]

-- rough definition of moves to be refined later
moves :: PieceType -> [Position]
moves Man = forwardDiagonal
moves King = diagonal

-- direction of play
getDirection :: PieceColor -> Int
getDirection White = 1
getDirection Black = -1

-- oldPosition -> oppositePiecePosition -> newPosition
jumpOver :: Position -> Position -> Position
jumpOver (a,b) (c,d) = if ((c - a) > 0) then (if ((d-b) > 0) then (c+1, d+1) else (c+1, d-1)) else (if ((d-b) > 0) then (c-1, d+1) else (c-1, d-1))

-- move generator, simple pawn, no castling
genMoveBoards :: Board -> Position -> [Board]
genMoveBoards board pos = case getField board pos of
  Nothing -> [] 
  Just piece -> map (flip (movePiece pos) board) $ genPieceMoves board pos piece 

genMoves :: Board -> Position -> [Position]
genMoves board pos = case getField board pos of
  Nothing -> [] 
  Just piece -> genPieceMoves board pos piece 

genPieceMoves :: Board -> Position -> Piece -> [Position]
genPieceMoves board pos (Piece King color) = concatMap (iterateDirection 1 pos board color) (moves King)
genPieceMoves board pos (Piece Man color) = [coord|v <- map (multPair (getDirection color)) (moves Man), let coord = addPair pos v, isOppositeColor board color coord]

concatWithNewLines :: String -> String -> String
concatWithNewLines first second = first ++ "\n\n" ++ second

showAllPossibleMovesFor :: Board -> Position -> String
showAllPossibleMovesFor board pos = foldl concatWithNewLines "" (map showBoard (genMoveBoards board pos))

isOppositeColor :: Board -> PieceColor -> Position -> Bool
isOppositeColor board color pos = isPositionInside pos && not (hasFieldColor color (getField board pos))

isFieldEmpty :: Board -> Position -> Bool
isFieldEmpty board pos = isPositionInside pos && (getField board pos) == Nothing

isOppositePiece :: Board -> PieceColor -> Position -> Bool
isOppositePiece board color pos = isPositionInside pos && hasFieldColor (getOppositeColor color) (getField board pos) 

-- gets moving vectors for rooks, queens and bishops, the first paramater pos is the 
-- position of the piece, the second the direction to iterate at
iterateDirection :: Int -> Position -> Board -> PieceColor -> Position -> [Position]
iterateDirection n pos board color position | isPositionOutside aimsAt = []
                             | otherwise = case getField board aimsAt of
                                             Nothing -> aimsAt:iterateDirection (n+1) pos board color position
                                             Just (Piece _ color2) -> if color==color2 then [] else [aimsAt]
  where aimsAt = addPair (multPair n position) pos

nextStates :: State -> [State]
nextStates (pieceColor, board) = [(getOppositeColor pieceColor, board')|pos<-getColorPositions pieceColor board, board'<-genMoveBoards board pos]
