module StartingStructures where
import Board
import Utils
import DataTypes


-- Init --
boardInitString :: String
boardInitString = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."

startingBoard :: Board
startingBoard = readBoard boardInitString



-- boards to test if getting possible moves works
testBoardString = "........\n..B...B.\n........\n..b...b.\n...w....\n....b...\n...w.w..\n........"
testBoard = readBoard testBoardString

testBoardString2 = "........\n..B.....\n........\n..b...b.\n...w....\n..B.b...\n...w.w..\n........"
testBoard2 = readBoard testBoardString2

testBoardString3 = ".b......\n..b.....\n........\n........\n........\n........\n.......B\n......W."
testBoard3 = readBoard testBoardString3

-- example: putStr $ showBoard $ doNextMove testBoard $ head $ generatePossibleMoves testBoard (6,5)
-- computer vs computer: putStrLn $ concatMap (("\n"++) . showBoard . snd) $ take 58 $ iterate doMinimaxMove (White, startingBoard)


