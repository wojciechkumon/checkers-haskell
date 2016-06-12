module DataTypes where

data Piece = Piece {pieceType :: PieceType, pieceColor :: PieceColor} deriving Eq
data PieceType = Man | King deriving (Eq, Show)
data PieceColor = White | Black deriving (Eq, Show)

type Field = Maybe Piece
type Board = [[Field]]
type Position = (Int, Int)



-- PieceColor to decide whose turn it is
type GameState = (PieceColor, Board)
type NextMove = ([Position], [Position])
data Move = Move (Position, Position) | Kill [Position] deriving (Show, Eq)


data GameTree = GameTree {gameState :: GameState, gameTrees :: [GameTree]} deriving Show
data NewGameTree = NewGameTree {newGameState :: GameState, maybeMove :: (Maybe Move), newGameTrees :: [NewGameTree]} deriving Show


type GameIO a = IO a

instance Show Piece where
  show (Piece Man White) = "w"
  show (Piece Man Black) = "b"
  show (Piece King White) = "W"
  show (Piece King Black) = "B"
