module Game where
import Board
import Utils
import DataTypes


playGame :: [(Board -> Board)] -> Board
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

testBoardString = "........\n..B...B.\n........\n..b...b.\n...w....\n....b...\n...w.w..\n........"
testBoard = readBoard testBoardString

testBoardString2 = "........\n..B.....\n........\n..b...b.\n...w....\n..B.b...\n...w.w..\n........"
testBoard2 = readBoard testBoardString2

testBoardString3 = ".b......\n..b.....\n........\n........\n........\n........\n.......B\n......W."
testBoard3 = readBoard testBoardString3



fld1 = Just (Piece King Black)
fld2 = Just (Piece Man White)
fld3 = Nothing
fld4 = Just (Piece Man Black)
piece1 = (read "B") :: Piece
-- example: putStr $ showBoard $ doNextMove testBoard $ head $ generatePossibleMoves testBoard (6,5)
-- computer vs computer: putStrLn $ concatMap (("\n"++) . showBoard . snd) $ take 58 $ iterate doMinimaxMove (White, startingBoard)
