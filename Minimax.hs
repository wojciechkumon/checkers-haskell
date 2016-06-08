module Minimax where

import Board
import Evaluator
import Moves

data GameTree = GameTree {state :: State, gameTrees :: [GameTree]}

maxTreeDepth :: Int
maxTreeDepth = 3

generateGameTree :: Int -> State -> GameTree
generateGameTree 0 state = GameTree state []
generateGameTree depth state | isLastState state = GameTree state []
                             | otherwise = GameTree state (map (generateGameTree (depth-1)) (getNextStates state))

evaluateState :: State -> Int
evaluateState = evaluateBoard . snd

-- checks end of the game, TODO check if all blocked
isLastState :: State -> Bool
isLastState state = (whitePoints == 0) || (blackPoints == 0)
  where (whitePoints, blackPoints) = analyzeBoard (snd state)


playMinimax :: GameTree -> Int
playMinimax (GameTree state []) = evaluateState state
playMinimax (GameTree (White,_) children) = maximum (map playMinimax children)
playMinimax (GameTree (Black,_) children) = minimum (map playMinimax children)
