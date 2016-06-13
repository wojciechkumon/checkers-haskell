module Conversions where
import DataTypes
import Board

-- converts 1-32 representation of move to 2-dim coords
convertToMove :: PDN -> Move
convertToMove (MovePDN (from,to)) = Move (readPosition from, readPosition to)
convertToMove (KillPDN list) = Kill (map readPosition list)

-- converts move to String, Move to for example 10-14, Kill to for example 27x18x11 (double kill here)
convertToPdnString :: Move -> String
convertToPdnString (Move (posFrom,posTo)) = convertPositionToPdn posFrom ++ "-" ++ convertPositionToPdn posTo
convertToPdnString (Kill [lastPosition]) = convertPositionToPdn lastPosition
convertToPdnString (Kill (x:xs)) = convertPositionToPdn x ++ "x" ++ convertToPdnString (Kill xs)

-- converts position (x,y) to number (1-32)
convertPositionToPdn :: Position -> String
convertPositionToPdn (x,y) = show ((x*4) + if (y `mod` 2) == 0 then (y `quot` 2) + 1 else ((y+1) `quot` 2))
