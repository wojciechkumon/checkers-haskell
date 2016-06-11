module Minimax where
import Board
import Evaluator
import Moves
import DataTypes


maxTreeDepth :: Int
maxTreeDepth = 5

generateGameTree :: Int -> GameState -> GameTree
generateGameTree 0 state = GameTree state []
generateGameTree depth state | isLastState state = GameTree state []
                             | otherwise = GameTree state (map (generateGameTree (depth-1)) (getNextStates state))

evaluateState :: GameState -> Int
evaluateState = evaluateBoard . snd

-- checks end of the game, TODO check if all blocked
isLastState :: GameState -> Bool
isLastState state = (whitePoints == 0) || (blackPoints == 0) || (isEndOfGame state)
  where (whitePoints, blackPoints) = analyzeBoard (snd state)


calculateMinimax :: GameTree -> Int
calculateMinimax (GameTree state []) = evaluateState state
calculateMinimax (GameTree (White,_) children) = maximum (map calculateMinimax children)
calculateMinimax (GameTree (Black,_) children) = minimum (map calculateMinimax children)

doMinimaxMove :: GameState -> GameState
doMinimaxMove state = case (generateGameTree maxTreeDepth state) of
                        GameTree foundState [] -> foundState
                        GameTree (color,_) forest -> snd $ getBestChoice color (compareColor color) $ 
                                                     map (\tree -> (calculateMinimax tree, gameState tree)) forest
        where compareColor White = (>)
              compareColor Black = (<)

getBestChoice :: PieceColor -> (Int -> Int -> Bool) -> [(Int, GameState)] -> (Int, GameState)
getBestChoice _ _ [val] = val
getBestChoice color comparator ((int,state):tail) | isWinningState color state = (int,state)
                                                  | otherwise = let (int2,state2) = getBestChoice color comparator tail in
                                                                     if comparator int int2 then (int,state) else (int2,state2)

isWinningState :: PieceColor -> GameState -> Bool
isWinningState color (_,board) = isEndOfGame (color, board)