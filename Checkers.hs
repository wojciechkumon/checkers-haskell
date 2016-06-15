import DataTypes
import Board
import Moves
import StartingStructures
import Strings
import Minimax
import Parser
import Conversions
import System.Environment
import Data.Maybe
import Text.ParserCombinators.Parsec

-- main which prints only computer's moves

-- accepts 1 or 0 arguments
-- if this argument == "b" then you start as black, else you start as white
main :: IO ()
main = do
  args <- getArgs
  appName <- getProgName
  putStrLn appName
  mapM_ putStrLn args
  case (listToMaybe args) of
    Just "b" -> handlePlay Black (startingState, "")
    otherwise -> handlePlay White (startingState, "")


-- function for recursive calls (one call == one move)
handlePlay :: PieceColor -> (GameState, String) -> IO ()
handlePlay playerColor ((color, board), info) = do
  putStr info
  if (isEndOfGame (color, board)) then
    if (color == White) then putStrLn blackWinsString else putStrLn whiteWinsString
  else
    if (playerColor == color) then
      do
        moveStr <- getLine
        playerMove playerColor (color, board) moveStr
    else handlePlay playerColor (nextComputerMove (color, board)) 


playerMove :: PieceColor -> GameState -> String -> IO ()
playerMove playerColor (color, board) moveStr = 
  case parse parsePDN parseErrorString moveStr  of
    Right move -> putStrLn moveStr >> handlePlay playerColor (nextPlayerMove (color, board) (convertToMove move))
    Left x -> handlePlay playerColor ((color, board), wrongMoveString)


nextPlayerMove :: GameState -> Move -> (GameState, String)
nextPlayerMove (color, board) move = 
   if (isCorrectMove (color, board) move) 
     then ((getOppositeColor color, (doNextMove board move)),"")
   else ((color, board), wrongMoveString)


nextComputerMove :: GameState -> (GameState, String)
nextComputerMove (color, board) = case (getMinimaxMove (color, board)) of
                           Nothing -> ((color, board), errorString)
                           Just move -> ((getOppositeColor color, (doNextMove board move)), (convertToPdnString move ++ "\n"))
