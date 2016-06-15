module Moves where
import Board
import Utils
import DataTypes


-- generates board after move, move must be correct
doNextMove :: Board -> Move -> Board 
doNextMove board (Move (oldPos,newPos)) = switchToKingIfPossible newPos $ movePiece oldPos newPos board
doNextMove board (Kill [oldPos,newPos]) =  switchToKingIfPossible newPos $ movePiece oldPos newPos (deletePiecesOnLine oldPos newPos board)
doNextMove board (Kill (oldPos:newerPos:nextPoses)) = doNextMove (movePiece oldPos newerPos (deletePiecesOnLine oldPos newerPos board)) (Kill (newerPos:nextPoses))

switchToKingIfPossible :: Position -> Board -> Board
switchToKingIfPossible (x,y) board | (x == 7) && (getField board (x,y) == (Just (Piece Man Black))) = updateBoard board (Just (Piece King Black)) (x,y)
                                   | (x == 0) && (getField board (x,y) == (Just (Piece Man White))) = updateBoard board (Just (Piece King White)) (x,y)
                                   | otherwise = board

-- deletes everything between positions
deletePiecesOnLine :: Position -> Position -> Board -> Board
deletePiecesOnLine (oldX,oldY) (newX,newY) board | ((oldX == newX) && (oldY == newY)) = board
                                                 | otherwise = deletePiecesOnLine (getNextPositionBetween (oldX,oldY) (newX,newY)) (newX,newY) (deletePiece board (getNextPositionBetween (oldX,oldY) (newX,newY)))

getNextPositionBetween :: Position -> Position -> Position
getNextPositionBetween (oldX,oldY) (newX,newY) = (oldX+(oneIfGreaterElseMinusOne newX oldX),oldY+(oneIfGreaterElseMinusOne newY oldY))


-- GENERATING POSSIBLE MOVES FOR POSITION

generatePossibleMoves :: Board -> Position -> [Move]
generatePossibleMoves board pos = generateMovesForField board pos (getField board pos)

generateMovesForField :: Board -> Position -> Field -> [Move]
generateMovesForField _ _ Nothing = []
generateMovesForField board pos (Just (Piece pieceType color)) = 
  if (not (null (generateCaptures board pos (Piece pieceType color)))) then 
       generateCaptures board pos (Piece pieceType color) 
  else generateNormalMoves board pos (Piece pieceType color)


-- recursively generates possible kills for position 
generateCaptures :: Board -> Position -> Piece -> [Move]
generateCaptures board pos (Piece Man color) = 
  removeNotLongestKillChains $ addRecursiveKills board (Piece Man color) $ 
  map (\position -> Kill [pos,position]) $ 
  filter (isFieldEmpty board) $ map (jumpOver pos) $ 
  filter (\position -> isOppositePiece board color position) $ 
  map (addPair pos) diagonal
generateCaptures board pos (Piece King color) =
  removeNotLongestKillChains $ addRecursiveKills board (Piece King color) $ 
  map (\afterJumpPos -> Kill [pos, afterJumpPos]) $
  mapToNextPositionsAfterKill board color $
  filter (\(enemyPos, nextPosition) -> isFieldEmpty board nextPosition) $
  map (\(position, enemyPos) -> (enemyPos, jumpOver position enemyPos)) $
  filter (\(position, enemyPos) -> isOppositePiece board color enemyPos) $
  (getSurroundingPositionPairs pos) ++
  (map (\position -> (position, getNextPosition pos position)) $
  generatePossiblePositions board pos (Piece King color))
      where getSurroundingPositionPairs position = map (\diagonalPos -> (position,diagonalPos)) $ filter isPositionInside $ map (\diagonalPos -> addPair diagonalPos position) diagonal



mapToNextPositionsAfterKill :: Board -> PieceColor -> [(Position, Position)] -> [Position]
mapToNextPositionsAfterKill board color positions = foldl (++) [] $
  map (\(enemyPos, newPos) -> iterateDirection 1 enemyPos board color (getDirection newPos enemyPos)) positions
    where getDirection (newX, newY) (oldX, oldY) = (oneIfGreaterElseMinusOne newX oldX, oneIfGreaterElseMinusOne newY oldY)

getNextPosition :: Position -> Position -> Position
getNextPosition (baseX,baseY) (curX,curY) = (curX+(oneIfGreaterElseMinusOne curX baseX),curY+(oneIfGreaterElseMinusOne curY baseY))

removeNotLongestKillChains :: [Move] -> [Move]
removeNotLongestKillChains moves = filter (\(Kill killsList) -> length killsList == maxKillMoves moves) moves
  where maxKillMoves killMoves = maximum $ map (\(Kill killsList) -> length killsList) killMoves

addRecursiveKills :: Board -> Piece -> [Move] -> [Move]
addRecursiveKills board piece moves = foldl (++) [] $ map (convertToNextKill board piece) moves
--addRecursiveKills board (Piece Man color) moves = foldl (++) [] $ map (convertToNextKill board (Piece Man color)) moves
--addRecursiveKills board (Piece King color) moves = foldl (++) [] $ map (convertToNextKill board (Piece King color)) moves

convertToNextKill :: Board -> Piece -> Move -> [Move]
convertToNextKill oldBoard piece (Kill killMoves) = 
  if not (null (generateCaptures (doNextMove oldBoard (Kill killMoves)) (last killMoves) piece)) then
       map (addNextKill (Kill killMoves)) $ generateCaptures (doNextMove oldBoard (Kill killMoves)) (last killMoves) piece
  else [Kill killMoves]

addNextKill :: Move -> Move -> Move
addNextKill (Kill currentKillMoves) (Kill (killHead:killTail)) = Kill (currentKillMoves++killTail)


generateNormalMoves :: Board -> Position -> Piece -> [Move]
generateNormalMoves board pos (Piece Man color) = 
  map (\position -> Move (pos, position)) $
  filter (\position -> isFieldEmpty board position) $ 
  map (\possibleMove -> addPair pos (multPair (getDirection color) possibleMove)) (getMovesDirections Man)
generateNormalMoves board pos (Piece King color) = map (\newPos -> Move (pos,newPos)) $ generatePossiblePositions board pos (Piece King color)

generatePossiblePositions :: Board -> Position -> Piece -> [Position]
generatePossiblePositions  board pos (Piece King color) = concatMap (iterateDirection 1 pos board color) diagonal


-- GENERATING ALL POSSIBLE MOVES FOR PLAYER
generatePossiblePlayerMoves :: Board -> PieceColor -> [Move]
generatePossiblePlayerMoves board color = filterBestMoves $ foldl (++) [] $ map (generatePossibleMoves board) $ getColorPositions color board

filterBestMoves :: [Move] -> [Move]
filterBestMoves moves = if not (null (filterOnlyKills moves)) then removeNotLongestKillChains (filterOnlyKills moves) else moves

filterOnlyKills :: [Move] -> [Move]
filterOnlyKills moves = filter (isKill) moves
                        where isKill (Kill _) = True
                              isKill _ = False

-- checks if move is possible
isCorrectMove  :: GameState -> Move -> Bool
isCorrectMove (color, board) move = elem move $ generatePossiblePlayerMoves board color

-- checks if it's end of the game
isEndOfGame :: GameState -> Bool
isEndOfGame (color, board) = null $ generatePossiblePlayerMoves board color


-- general possible moves for pieces
getMovesDirections :: PieceType -> [Position]
getMovesDirections Man = forwardDiagonal
getMovesDirections King = diagonal

-- diagonal moves for King and captures
diagonal :: [Position]
diagonal = [(1,1),(-1,-1),(1,-1),(-1,1)]

-- forward diagonal moves for Man
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


isOppositeColor :: Board -> PieceColor -> Position -> Bool
isOppositeColor board color pos = isPositionInside pos && not (hasFieldColor color (getField board pos))

isFieldEmpty :: Board -> Position -> Bool
isFieldEmpty board pos = isPositionInside pos && (getField board pos) == Nothing

isOppositePiece :: Board -> PieceColor -> Position -> Bool
isOppositePiece board color pos = isPositionInside pos && hasFieldColor (getOppositeColor color) (getField board pos) 

-- gets moving vectors for rooks, queens and bishops, the first paramater pos is the 
-- position of the piece, the second the direction to iterate at
iterateDirection :: Int -> Position -> Board -> PieceColor -> Position -> [Position]
iterateDirection n pos board color position | isPositionOutside getNextPositionInIteration = []
                                            | otherwise = case getField board getNextPositionInIteration of
                                                            Nothing -> getNextPositionInIteration:iterateDirection (n+1) pos board color position
                                                            Just _ -> []
  where getNextPositionInIteration = addPair (multPair n position) pos

getNextStates :: GameState -> [GameState]
getNextStates (color,board) = map (\brd -> (getOppositeColor color, brd)) $ map (doNextMove board) $ generatePossiblePlayerMoves board color

getNextMoveStates :: GameState -> [(GameState, (Maybe Move))]
getNextMoveStates (color,board) = map (\(brd,mv) -> ((getOppositeColor color, brd),(Just mv))) $ map (\mv -> (doNextMove board mv,mv)) $ generatePossiblePlayerMoves board color
