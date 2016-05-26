module Game where
import Board

type Move = Board -> Board
type Game = [Move]

playGame :: Game -> Board
playGame = applyAll initialBoard