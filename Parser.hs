module Parser where
import Board
import Moves
import Game
import DataTypes
import System.Environment
import Control.Monad.Trans.State.Lazy
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.ParserCombinators.Parsec.Error
import System.IO
import Data.Maybe
import Control.Monad.IO.Class

--- TODO przetestowac
-- uproszczony "parser" PDN
-- ruch Int-Int
-- bicie [Intx]Int

data PDN =   MovePDN (Int,Int) -- pozycja startowa i koncowa
           | KillPDN [Int]     -- pozycja startowa to glowa, pozniej kolejne pozycje
           deriving (Show,Eq)

-- instance Show PDN where
--   show (Move (a,c)) = (show a)++ "-"++ (show c)
--   show (Kill [p]) = (show p)
--   show (Kill (p:ps)) = (show p)++ "x" ++ show (Kill ps)


parsePos :: Parser Int
parsePos = do
            x <- int
            if (x<1 || x>32) then
              unexpected "Tylko liczby od 1-32"
            else
              return x

parseMove = do
            x1 <- parsePos
            (char '-')
            x2 <- parsePos
            eof
            return $ MovePDN (x1,x2)

parseKill = do
            x1 <- sepBy (parsePos) (char 'x')
            eof
            if (length x1) > 1 then
              return $ KillPDN x1
            else
              unexpected "start i koniec minimum"

parsePDN =  try parseMove <|> parseKill


play :: String -> GameIO ()
play line = do
  case parse parsePDN "sPDN err" line of
    Right move -> do
                    hPutStrLn stderr $ "ruch = " ++ (show move)
    Left x -> fail $ show x
  putStrLn "11-15" >> hFlush stdout -- konkretny ruch trzeba wygenerowac

doPlay :: GameIO ()
doPlay = getContents >>= (mapM_ play) . lines

--doPlay state = iterateUntilEnd (play  ) state
                              

--iterate (nextMove playerColor) (White, startingBoard)
nextMove :: PieceColor -> GameState -> GameState
nextMove playerColor (stateColor, board) | (playerColor == stateColor) = nextPlayerMove (stateColor, board)
                                         | otherwise = nextComputerMove (stateColor, board)
-- TODO
nextPlayerMove :: GameState -> GameState
nextPlayerMove (color, board) = (color, board) 

nextComputerMove :: GameState -> GameState
nextComputerMove state = state--case (generateGameTree maxTreeDepth state) of
--                           GameTree newState [] -> newState
--                           GameTree (color,_) children -> 


