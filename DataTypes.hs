module DataTypes where

-- moves with input format
data PDN =   MovePDN (Int,Int) -- board has field from 1 to 32
           | KillPDN [Int]   
           deriving (Show,Eq)

data Piece = Piece {pieceType :: PieceType, pieceColor :: PieceColor} deriving Eq
data PieceType = Man | King deriving (Eq, Show)
data PieceColor = White | Black deriving (Eq, Show)

type Field = Maybe Piece
type Board = [[Field]]
type Position = (Int, Int)



-- PieceColor to decide whose turn it is
type GameState = (PieceColor, Board)

-- move with coords format
data Move = Move (Position, Position) | Kill [Position] deriving (Show, Eq)

data GameTree = GameTree {gameState :: GameState, maybeMove :: (Maybe Move), gameForest :: [GameTree]} deriving Show



instance Show Piece where
  show (Piece Man White) = "w"
  show (Piece Man Black) = "b"
  show (Piece King White) = "W"
  show (Piece King Black) = "B"

-- format: "w" "W" "b" "B" for white, white king, black, black king
instance Read Piece where
  readsPrec _ value = 
    tryParse [("w", (Piece Man White)), ("b", (Piece Man Black)), 
      ("W", (Piece King White)), ("B", (Piece King Black))]
    where tryParse [] = []
          tryParse ((attempt, result):xs) = 
            if (take (length attempt) value) == attempt then 
              [(result, drop (length attempt) value)]
            else tryParse xs

