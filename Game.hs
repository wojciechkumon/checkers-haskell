module Game where
import Board
import Utils

type Move = Board -> Board
type Game = [Move]

playGame :: Game -> Board
playGame = applyAll startingBoard


-- Init --
boardInitString :: String
boardInitString = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
startingBoard :: Board
startingBoard = readBoard boardInitString
startingBoardList :: [Board]
startingBoardList = [startingBoard]

boardStringAfterFirstMove :: String
boardStringAfterFirstMove = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n...w....\nw...w.w.\n.w.w.w.w\nw.w.w.w."
boardAfterFirstMove :: Board
boardAfterFirstMove = readBoard boardStringAfterFirstMove
boardsAfterFirstMoveList :: [Board]
boardsAfterFirstMoveList = [startingBoard, boardAfterFirstMove]

testBoardString = "........\n..B...b.\n........\n......b.\n........\n......b.\n.....w..\n........"
testBoard = readBoard testBoardString

fld1 = Just (Piece King Black)
fld2 = Just (Piece Man White)
fld3 = Nothing
fld4 = Just (Piece Man Black)
piece1 = (read "B") :: Piece
