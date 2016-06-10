module Checkers where
import Board
import Moves
import Utils
import Game
import Evaluator
import Minimax

startGameAs :: PieceColor -> IO ()
startGameAs playerColor = putStrLn $ concatMap (("\n"++) . showBoard . snd) $ take 100 $ iterateUntilEnd (nextMove playerColor) (White, startingBoard)


nextMove :: PieceColor -> State -> State
nextMove playerColor (stateColor, board) | (playerColor == stateColor) = nextPlayerMove (stateColor, board)
                                         | otherwise = nextComputerMove (stateColor, board)

-- TODO
nextPlayerMove :: State -> State
nextPlayerMove (color, board) = (color, board) 

nextComputerMove :: State -> State
nextComputerMove state = case (generateGameTree maxTreeDepth state) of
                           GameTree newState [] -> newState
                           GameTree (color,_) children -> 