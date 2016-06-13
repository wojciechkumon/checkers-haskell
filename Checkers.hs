import DataTypes
import Board
import Moves
import Utils
import StartingStructures
import Strings
import Evaluator
import Minimax
import Parser
import Conversions
import System.IO
import System.Environment
import Data.Maybe



import Text.ParserCombinators.Parsec





--main :: IO ()
--main = do
--  args <- getArgs
--  progName <- getProgName
--  mapM_ putStrLn args
--  putStrLn progName
--  let args = ["w"] -- do zakomentowania w programmie
--  case (listToMaybe args) of
--    Just "b" -> doPlay
--    Just "w" -> putStrLn "22-18" >> hFlush stdout >> doPlay -- białe wykonują pierwszy ruch
--    Nothing -> doPlay


main2 :: IO ()
main2 = do
    let initialBoard = startingBoard
    managePlay White ((White, initialBoard), "")

managePlay :: PieceColor -> (GameState, String) -> IO ()
managePlay playerColor ((color, board), info) = do
  putStr (info ++ "\n" ++ showBoard board)
  if (isEndOfGame (color, board)) then
    if (color == White) then putStrLn blackWinsString else putStrLn whiteWinsString
  else
    if (playerColor == color) then
      do
        moveStr <- getLine
        playerMove playerColor (color, board) moveStr
    else managePlay playerColor (nextComputerMove (color, board)) 
 

playerMove :: PieceColor -> GameState -> String -> IO ()
playerMove playerColor (color, board) moveStr = 
  case parse parsePDN "parse error" moveStr  of
    Right move -> putStrLn moveStr >> managePlay playerColor (nextPlayerMove (color, board) (convertToMove move))
    Left x -> managePlay playerColor ((color, board), wrongMoveString)

-- TODO
nextPlayerMove :: GameState -> Move -> (GameState, String)
nextPlayerMove (color, board) move = 
   if (isCorrectMove (color, board) move) 
     then ((getOppositeColor color, (doNextMove board move)),"")
   else ((color, board), wrongMoveString)


nextComputerMove :: GameState -> (GameState, String)
nextComputerMove (color, board) = case (getMinimaxMove (color, board)) of
                           Nothing -> ((color, board), "error")
                           Just move -> ((getOppositeColor color, (doNextMove board move)), convertToPdnString move)


-- showExampleGameComputerVsComputer
