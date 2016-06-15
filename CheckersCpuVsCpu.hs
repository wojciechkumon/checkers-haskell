import DataTypes
import Board
import Moves
import StartingStructures
import Minimax

-- main which allows watching game between two computers

main :: IO ()
main = runCpuVsCpu

-- example CPU vs CPU game
runCpuVsCpu = putStrLn $ concatMap (("\n"++) . showBoard . snd) $ takeWhileItsNotEndOfTheGame $ iterate doMinimaxMove (White, startingBoard)
  where takeWhileItsNotEndOfTheGame (state:xs) = if (isEndOfGame state) then [state] else state:takeWhileItsNotEndOfTheGame xs
