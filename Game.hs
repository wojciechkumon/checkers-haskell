module Game where
import Board
import Utils

type Move = Board -> Board
type Game = [Move]

playGame :: Game -> Board
playGame = applyAll startingBoard


-- Init --
boardInitString = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
startingBoard = readBoard boardInitString

testBoardString = "........\n..B...b.\n........\n......b.\n........\n......b.\n.....w..\n........"
testBoard = readBoard testBoardString

fld1 = Just (Piece King Black)
fld2 = Just (Piece Man White)
fld3 = Nothing
fld4 = Just (Piece Man Black)
piece1 = (read "B") :: Piece
