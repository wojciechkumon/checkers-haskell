module Checkers where
import Board
import Moves
import Utils
import Game

-- Init --

boardInitString = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
startingBoard = readBoard boardInitString

testBoardString = "........\n..B...b.\n........\n....W...\n........\n........\n.....w..\n........"
testBoard = readBoard testBoardString

fld1 = Just (Piece King Black)
fld2 = Just (Piece Man White)
fld3 = Nothing
fld4 = Just (Piece Man Black)
piece1 = (read "B")::Piece