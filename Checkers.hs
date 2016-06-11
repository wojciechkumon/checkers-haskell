import DataTypes
import Board
import Moves
import Utils
import Game
import Evaluator
import Minimax
import Parser
import System.IO
import System.Environment
import Data.Maybe

--startGameAs :: PieceColor -> IO ()
--startGameAs playerColor = putStrLn $ concatMap (("\n"++) . showBoard . snd) $ take 100 $ iterateUntilEnd (nextMove playerColor) (White, startingBoard)




main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  mapM_ putStrLn args
  putStrLn progName
  let args = ["w"] -- do zakomentowania w programmie
  case (listToMaybe args) of
    Just "b" -> doPlay
    Just "w" -> putStrLn "22-18" >> hFlush stdout >> doPlay -- białe wykonują pierwszy ruch
    Nothing -> doPlay
