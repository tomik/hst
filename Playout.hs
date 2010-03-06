
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

getJumpFromLastPeg :: Maybe Peg -> Board -> StdGen -> (Pos, StdGen)
getJumpFromLastPeg Nothing board gen = (noPos, gen)
getJumpFromLastPeg (Just peg) board gen =

    let connected = filter (isLegalMove board) $ 
                    map (\pos -> mkPeg (getCol pos) (getRow pos) (pegColor peg)) $ 
                    jumps (pegPos peg)
    in
        if length connected == 0 then
            --trace (show board ++ (show $ getConnectedPegs board peg) ++ show peg)
            (noPos, gen)
        else
            let (index, newGen) = randomR (0, length connected - 1) gen
                neighbor = connected !! index
            in (pegPos neighbor, newGen)
    
heurJump :: HeurFunc
heurJump board =
    do
        --try to jump from peg
        gameState <- get
        let (pos, newGen) = getJumpFromLastPeg (getLastByColor board $ bdToPlay board) board $ gsGen gameState 
        put gameState {gsGen=newGen}
        return $ pos

mkHeurs :: [Heur]
mkHeurs = [Heur heurJump 1.0, Heur heurEmpty 1.0]

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
    let games = evalState (sequence $ replicate n (doPlayout board)) gen
    in map gmWinner games

--generic playout dispatcher
doPlayout :: Board -> State StdGen Game
doPlayout board =
    do
        gen <- get
        let gameState = initGameState board gen
        let (game, newGameState) = runState (simplePlayout board mkHeurs) gameState
        -- we are interested only in random generator
        put $ gsGen newGameState
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
    trace (show board)
     -}
    return $ mkGame (gmWinner game) (peg:(gmMoves game))

makeMove :: Board -> Move -> Board
makeMove board peg | pegPos peg == noPos = board {bdToPlay = oppColor $ bdToPlay board}
makeMove board peg | otherwise = placePeg board peg

