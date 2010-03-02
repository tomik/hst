
module Playout where

import Random
import Debug.Trace
import Control.Monad.State
import Data.List

import Board
import Utils (shuffle)

type Move = Peg
type Moves = [Move]
data Game = Game {gmWinner :: Maybe Color, gmMoves :: Moves} deriving (Show, Eq)

mkGame winner moves = Game {gmWinner = winner, gmMoves = moves} 

-- ==============================
-- Heuristics
-- ==============================

--all gamestate information (for heuristic)
--TODO
--data GameState = forall gen . (RandomGen gen) => GameState {gsGen :: gen, gsEmpty :: [Pos]}
data GameState = GameState {gsGen :: StdGen, gsEmpty :: [Pos]}
--type Heur = Board -> HeurState -> gen -> (HeurState, gen)
type HeurFunc = Board -> State GameState Pos
data Heur = Heur HeurFunc Float

dispatchHeurs :: [Heur] -> Board -> State GameState Pos
dispatchHeurs [] _ = return noPos
dispatchHeurs ((Heur heurFunc bias):heurs) board = 
    do
        gameState <- get
        --throw a dice to see whether apply heur
        let (dice, newGen) = randomR (0.0, 1.0) (gsGen gameState)    
        put gameState {gsGen=newGen} 
        if dice <= bias then 
            do 
                pos <- heurFunc board 
                if pos /= noPos then return pos else dispatchHeurs heurs board
            else 
                dispatchHeurs heurs board

heurEmpty :: HeurFunc
--heurEmpty board = error "no where to play - empty list"
heurEmpty board = 
    let pos2peg = (\pos -> mkPeg (getRow pos) (getCol pos) (bdToPlay board))
    in 
    do
        gameState <- get
        let (trash, rest) = break (\pos -> isLegalMove board (pos2peg pos)) (gsEmpty gameState)
        --opponent can play some of the moves we consider trash
        let oppCanPlay = filter (isEmptySquare board) trash 
        --update gamestate
        put $ gameState {gsEmpty=(oppCanPlay ++ rest)}
        if length(rest) == 0 then return noPos else return $ rest !! 0

heurJump :: HeurFunc
heurJump board = 
    do
        --select random pos and play it, if it is legal keima 
        gameState <- get 
        let (pos, newGen) = getRandomPos board (gsGen gameState)
        let peg = mkPeg (getRow pos) (getCol pos) (bdToPlay board)
        put gameState {gsGen=newGen}
        if (isLegalMove board peg) && 
            (not . null $ getConnectedPegs board peg) then 
                return $ pegPos peg 
            else 
                return noPos

mkHeurs :: [Heur]
mkHeurs = [Heur heurJump 0.2, Heur heurEmpty 1.0]

-- ==============================
-- Generic playouts and simplePlayout
-- ==============================

--initGameState :: (RandomGen gen) => Board -> gen -> GameState
initGameState :: Board -> StdGen -> GameState
initGameState board gen = 
        let emptyPos = getAllEmptyPos board
            (shuffled, newGen) = shuffle emptyPos gen
        in GameState{gsEmpty=shuffled, gsGen=newGen}

--runs given number of playouts and returns statistics
--doPlayouts :: (RandomGen gen) => Board -> gen -> Int -> [Maybe Color] 
doPlayouts :: Board -> StdGen -> Int -> [Maybe Color] 
doPlayouts board gen n = 
    let gameState = initGameState board gen 
        games = evalState (sequence $ replicate n (doPlayout board)) gameState
    in map gmWinner games

--generic playout dispatcher
doPlayout :: Board -> State GameState Game
doPlayout board = 
    do 
    game <- simplePlayout board mkHeurs
    return game

--simplePlayout with heuristics 
simplePlayout :: Board -> [Heur] -> State GameState Game 
simplePlayout board heurs | hasWinner board = return $ mkGame (getWinner board) []
simplePlayout board heurs | isDraw board = return $ mkGame Nothing []
simplePlayout board heurs | otherwise = 
    do 
    gameState <- get
    pos <- dispatchHeurs heurs board 
    let peg = mkPeg (getRow pos) (getCol pos) (bdToPlay board) 
    let newBoard = makeMove board peg 
    game <- simplePlayout newBoard heurs
    {- t1142c
     -}
    trace (show board)
        return $ mkGame (gmWinner game) (peg:(gmMoves game))

makeMove :: Board -> Move -> Board
makeMove board peg | pegPos peg == noPos = board {bdToPlay = oppColor $ bdToPlay board}
makeMove board peg | otherwise = placePeg board peg

