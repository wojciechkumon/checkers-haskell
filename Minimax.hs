module Minimax where
import BoardEvaluator
import Moves
import Board
import DataTypes


-- module to calculate current best move based on minimax algorithm

data GameTree = GameTree {gameState :: GameState, maybeMove :: (Maybe Move), gameForest :: [GameTree]} deriving Show

maxTreeDepth :: Int
maxTreeDepth = 5

getMaybeMove :: GameTree -> (Maybe Move)
getMaybeMove (GameTree _ maybeMove _) = maybeMove

generateGameTree :: Int -> GameState -> GameTree
generateGameTree depth state = genGameTree depth (state,Nothing)

genGameTree :: Int -> (GameState, (Maybe Move)) -> GameTree
genGameTree 0 (state, maybeMove) = GameTree state maybeMove []
genGameTree depth (state, maybeMove) | isLastState state = GameTree state maybeMove []
                                     | otherwise = GameTree state maybeMove $ map (genGameTree (depth-1)) $ getNextMoveStates state

calculateMinimax :: GameTree -> Int
calculateMinimax (GameTree state _ []) = evaluateBoard $ snd state
calculateMinimax (GameTree (White,_) _ children) = maximum (map calculateMinimax children)
calculateMinimax (GameTree (Black,_) _ children) = minimum (map calculateMinimax children)

doMinimaxMove :: GameState -> GameState
doMinimaxMove state = case (generateGameTree maxTreeDepth state) of
                        GameTree foundState _ [] -> foundState
                        GameTree (color,_) _ forest -> snd $ getBestChoice color (compareColor color) $ 
                                                     map (\tree -> (calculateMinimax tree, gameState tree)) forest
        where compareColor White = (>)
              compareColor Black = (<)

getMinimaxMove :: GameState -> (Maybe Move)
getMinimaxMove state = case (generateGameTree maxTreeDepth state) of
                        GameTree _ maybeMove [] -> maybeMove
                        GameTree (color,_) maybeMove forest -> getMaybeMoveFromTuple $ getBestMove color (compareColor color) $ 
                                                     map (\tree -> (calculateMinimax tree, gameState tree, getMaybeMove tree)) forest
        where compareColor White = (>)
              compareColor Black = (<)
              getMaybeMoveFromTuple (_,_,maybeMove) = maybeMove

getBestMove :: PieceColor -> (Int -> Int -> Bool) -> [(Int, GameState, (Maybe Move))] -> (Int, GameState, (Maybe Move))
getBestMove _ _ [val] = val
getBestMove color comparator ((int,state,maybeMove):tail) | isWinningState color state = (int,state,maybeMove)
                                                          | otherwise = let (int2,state2,maybeMove2) = getBestMove color comparator tail in
                                                                     if comparator int int2 then (int,state,maybeMove) else (int2,state2,maybeMove2)

-- checks end of the game, TODO check if all blocked
isLastState :: GameState -> Bool
isLastState state = (whitePoints == 0) || (blackPoints == 0) || (isEndOfGame state)
  where (whitePoints, blackPoints) = analyzeBoard (snd state)

getBestChoice :: PieceColor -> (Int -> Int -> Bool) -> [(Int, GameState)] -> (Int, GameState)
getBestChoice _ _ [val] = val
getBestChoice color comparator ((int,state):tail) | isWinningState color state = (int,state)
                                                  | otherwise = let (int2,state2) = getBestChoice color comparator tail in
                                                                     if comparator int int2 then (int,state) else (int2,state2)

isWinningState :: PieceColor -> GameState -> Bool
isWinningState playerColor (_,board) = isEndOfGame (getOppositeColor playerColor, board)
