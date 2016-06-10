module Moves where
import Board
import Utils

-- PieceColor to decide whose turn it is
type State = (PieceColor, Board)

-- (newPosition, listOfCapturedOpponetsPiecesPostions)
type NextMove = (Position, [Position])


-- moves generator, generates all possible boards for position
--genMoveBoards :: Board -> Position -> [Board]
--genMoveBoards board pos = case getField board pos of
--  Nothing -> [] 
--  Just piece -> map (flip (movePiece pos) board) $ genPieceMoves board pos piece 


-- generates board after move, move must be correct
doNextMove :: Board -> Position -> Position -> Board 
doNextMove board posFrom posTo = movePiece posFrom posTo (deleteCaptures board (getNextMove board posFrom posTo))

deleteCaptures :: Board -> NextMove -> Board
deleteCaptures board (_, []) = board
deleteCaptures board (pos, (head:captures)) = deleteCaptures (deletePiece board head) (pos, captures) 

-- checks if move is possible for piece
isMovePossible :: Board -> Position -> Position -> Bool
isMovePossible board posFrom posTo = (filter (\(pos, _) -> pos == posTo) (genMoves board posFrom)) /= []

getNextMove :: Board -> Position -> Position -> NextMove
getNextMove board posFrom posTo = head (filter (\(pos, _) -> pos == posTo) (genMoves board posFrom))

-- moves generator, generates all possible moves for position
genMoves :: Board -> Position -> [NextMove]
genMoves board pos = case getField board pos of
  Nothing -> [] 
  Just piece -> genPieceMoves board pos piece

genPieceMoves :: Board -> Position -> Piece -> [NextMove]
--genPieceMoves board pos (Piece King color) = concatMap (iterateDirection 1 pos board color) (getPossibleMoves King)
--genPieceMoves board pos (Piece Man color) = 
  --[coord|v <- toNextMoves (map (multPair (getDirection color)) (getPossibleMoves Man)), let coord = addPair pos v, isOppositeColor board color coord]
genPieceMoves board pos (Piece pieceType color) = 
  if (isCapturePossible board pos color) then 
    getCaptures board pos (Piece pieceType color) else 
    toNextMoves $ filter (\position -> isFieldEmpty board position) $ 
    map (\possibleMove -> addPair (pos) (multPair (getDirection color) possibleMove)) (getPossibleMoves pieceType)

-- checks if any capture is possible for position, PieceColor = color of piece
isCapturePossible :: Board -> Position -> PieceColor -> Bool
isCapturePossible board pos color = 
  filter (isFieldEmpty board) (map (jumpOver pos) (filter (\position -> isOppositePiece board color position) (map (addPair pos) diagonal))) /= []

getCaptures :: Board -> Position -> Piece -> [NextMove]
getCaptures board pos (Piece pieceType pieceColor) =
  filter (\(newPos, [_]) -> isFieldEmpty board newPos) 
  (map (\capturedPosition -> (jumpOver pos capturedPosition, [capturedPosition])) 
  (filter (\position -> isOppositePiece board pieceColor position) 
  (map (addPair pos) diagonal)))

addCaptures :: [NextMove] -> [NextMove]
addCaptures [] = []
addCaptures nextMoves = nextMoves

toNextMoves :: [Position] -> [NextMove]
toNextMoves [] = []
toNextMoves positions = map (\pos -> (pos, [])) positions

-- general possible moves for pieces
getPossibleMoves :: PieceType -> [Position]
getPossibleMoves Man = forwardDiagonal
getPossibleMoves King = diagonal

-- diagonal moves for King and captures
diagonal :: [Position]
diagonal = [(1,1),(-1,-1),(1,-1),(-1,1)]

-- diagonal moves for Man
forwardDiagonal :: [Position]
forwardDiagonal = [(-1,1), (-1,-1)]

-- direction of play
getDirection :: PieceColor -> Int
getDirection White = 1
getDirection Black = -1

-- oldPosition -> oppositePiecePosition -> newPosition
jumpOver :: Position -> Position -> Position
jumpOver (a,b) (c,d) = if ((c - a) > 0) then 
  (if ((d-b) > 0) then (c+1, d+1) else (c+1, d-1))
  else (if ((d-b) > 0) then (c-1, d+1) else (c-1, d-1))


concatWithNewLines :: String -> String -> String
concatWithNewLines first second = first ++ "\n\n" ++ second

--showAllPossibleMovesFor :: Board -> Position -> String
--showAllPossibleMovesFor board pos = foldl concatWithNewLines "" (map showBoard (genMoveBoards board pos))

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

--nextStates :: State -> [State]
--nextStates (pieceColor, board) = [(getOppositeColor pieceColor, board')|pos<-getColorPositions pieceColor board, board'<-genMoveBoards board pos]


getNextStates :: State -> [State]
getNextStates (color,board) = []

iterateUntilEnd :: (a -> a) -> a -> [a]
iterateUntilEnd f a = a : iterateUntilEnd f (f a)