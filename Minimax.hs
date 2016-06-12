module Minimax where
import Board
import Evaluator
import Moves
import DataTypes


getMaybeMove :: NewGameTree -> (Maybe Move)
getMaybeMove (NewGameTree _ maybeMove _) = maybeMove

maxTreeDepth :: Int
maxTreeDepth = 3


newGenerateGameTree :: Int -> GameState -> NewGameTree
newGenerateGameTree i state = newGenGameTree i (state,Nothing)

newGenGameTree :: Int -> (GameState, (Maybe Move)) -> NewGameTree
newGenGameTree 0 (state, maybeMove) = NewGameTree state maybeMove []
newGenGameTree depth (state, maybeMove) | isLastState state = NewGameTree state maybeMove []
                                        | otherwise = NewGameTree state maybeMove $ map (newGenGameTree (depth-1)) $ getNextMoveStates state

newCalculateMinimax :: NewGameTree -> Int
newCalculateMinimax (NewGameTree state _ []) = evaluateState state
newCalculateMinimax (NewGameTree (White,_) _ children) = maximum (map newCalculateMinimax children)
newCalculateMinimax (NewGameTree (Black,_) _ children) = minimum (map newCalculateMinimax children)

newDoMinimaxMove :: GameState -> GameState
newDoMinimaxMove state = case (newGenerateGameTree maxTreeDepth state) of
                        NewGameTree foundState _ [] -> foundState
                        NewGameTree (color,_) _ forest -> snd $ getBestChoice color (compareColor color) $ 
                                                     map (\tree -> (newCalculateMinimax tree, newGameState tree)) forest
        where compareColor White = (>)
              compareColor Black = (<)

getMinimaxMove :: GameState -> (Maybe Move)
getMinimaxMove state = case (newGenerateGameTree maxTreeDepth state) of
                        NewGameTree _ maybeMove [] -> maybeMove
                        NewGameTree (color,_) maybeMove forest -> getMaybeMoveFromTuple $ getBestMove color (compareColor color) $ 
                                                     map (\tree -> (newCalculateMinimax tree, newGameState tree, getMaybeMove tree)) forest
        where compareColor White = (>)
              compareColor Black = (<)

getBestMove :: PieceColor -> (Int -> Int -> Bool) -> [(Int, GameState, (Maybe Move))] -> (Int, GameState, (Maybe Move))
getBestMove _ _ [val] = val
getBestMove color comparator ((int,state,maybeMove):tail) | isWinningState color state = (int,state,maybeMove)
                                                          | otherwise = let (int2,state2,maybeMove2) = getBestMove color comparator tail in
                                                                     if comparator int int2 then (int,state,maybeMove) else (int2,state2,maybeMove2)

getMaybeMoveFromTuple :: (Int, GameState, (Maybe Move)) -> (Maybe Move)
getMaybeMoveFromTuple (_,_,maybeMove) = maybeMove




-- TODO to remove old functions not returning Move

generateGameTree :: Int -> GameState -> GameTree
generateGameTree 0 state = GameTree state []
generateGameTree depth state | isLastState state = GameTree state []
                             | otherwise = GameTree state $ map (generateGameTree (depth-1)) $ getNextStates state

evaluateState :: GameState -> Int
evaluateState = evaluateBoard . snd

-- checks end of the game, TODO check if all blocked
isLastState :: GameState -> Bool
isLastState state = (whitePoints == 0) || (blackPoints == 0) || (isEndOfGame state)
  where (whitePoints, blackPoints) = analyzeBoard (snd state)


calculateMinimax :: GameTree -> Int
calculateMinimax (GameTree state []) = evaluateState state
calculateMinimax (GameTree (White,_) children) = maximum (map calculateMinimax children)
calculateMinimax (GameTree (Black,_) children) = minimum (map calculateMinimax children)

doMinimaxMove :: GameState -> GameState
doMinimaxMove state = case (generateGameTree maxTreeDepth state) of
                        GameTree foundState [] -> foundState
                        GameTree (color,_) forest -> snd $ getBestChoice color (compareColor color) $ 
                                                     map (\tree -> (calculateMinimax tree, gameState tree)) forest
        where compareColor White = (>)
              compareColor Black = (<)

getBestChoice :: PieceColor -> (Int -> Int -> Bool) -> [(Int, GameState)] -> (Int, GameState)
getBestChoice _ _ [val] = val
getBestChoice color comparator ((int,state):tail) | isWinningState color state = (int,state)
                                                  | otherwise = let (int2,state2) = getBestChoice color comparator tail in
                                                                     if comparator int int2 then (int,state) else (int2,state2)

isWinningState :: PieceColor -> GameState -> Bool
isWinningState color (_,board) = isEndOfGame (color, board)