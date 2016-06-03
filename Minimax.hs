module Minimax where

import Board
import Evaluator
import Moves


data GameTree = GameTree {state :: State, gameTrees :: [GameTree]}

maxDepth :: Int
maxDepth = 3


generateGameTree :: Int -> State -> GameTree
generateGameTree 0 state = GameTree state []
generateGameTree depth state

--isLastState :: State -> Bool
--isLastState state = 

