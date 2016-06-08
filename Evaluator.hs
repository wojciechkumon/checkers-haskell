module Evaluator where
import Board
import Utils

-- value > 0 means that white player has more points
evaluateBoard :: Board -> Int
evaluateBoard board = let (pointsWhite, pointsBlack) = analyzeBoard board in pointsWhite-pointsBlack

-- man values depends on position (board row) range 5-12
-- king values are fixed
getPieceValue :: Piece -> Int -> Int
getPieceValue (Piece Man Black) rowIndex = 5 + rowIndex
getPieceValue (Piece Man White) rowIndex = 12 - rowIndex
getPieceValue (Piece King _) _ =  25

-- aggregated values for two players Board -> (pointsWhite, pointsBlack)
analyzeBoard :: Board -> (Int,Int) -- (0,0,0) == (pointsWhite, pointsBlack,currentRow)
analyzeBoard board = takePairFromThree $ foldl (\(pw,pb,rowIndex) row -> addPairAndNextRow (pw,pb,rowIndex) (analyzeRow row rowIndex)) (0,0,0) board 
  where takePairFromThree (first,second,third) = (first,second)
        addPairAndNextRow (pw,pb,rowIndex) (pw2,pb2) = (pw+pw2,pb+pb2,rowIndex+1)


-- row -> rowIndex -> (pointsWhite, pointsBlack)
analyzeRow :: [Field] -> Int -> (Int, Int)
analyzeRow row rowIndex = foldl (addValue rowIndex) (0,0) row
  where addValue _ points Nothing = points -- rowIndex -> points -> field -> newPoints
        addValue rowIndex (pointsWhite,pointsBlack) (Just (Piece pieceType Black)) = (pointsWhite, pointsBlack + getPieceValue (Piece pieceType Black) rowIndex)
        addValue rowIndex (pointsWhite,pointsBlack) (Just (Piece pieceType White)) =  (pointsWhite + getPieceValue (Piece pieceType White) rowIndex, pointsBlack)
