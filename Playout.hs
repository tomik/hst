
module Playout where

import Random
import Debug.Trace
import Control.Monad.State
import Data.List

import Board

type Move = Peg
type Moves = [Move]
data Game = Game {gmWinner :: Maybe Color, gmMoves :: Moves} deriving (Show, Eq)

mkGame winner moves = Game {gmWinner = winner, gmMoves = moves} 

shuffle :: (RandomGen gen) => [a] -> State gen [a] 
shuffle l = shuffleAcc l []

shuffleAcc :: (RandomGen gen) => [a] -> [a] -> State gen [a]
shuffleAcc [] acc = return acc
shuffleAcc l acc = 
    do 
    gen <- get
    let (index, newGen) = randomR (0, length l - 1) gen
    put newGen
    let (begin, pivot:end) = splitAt index l
    shuffleAcc (begin ++ end) (pivot:acc)

-- ==============================
-- Heuristics
-- ==============================

class Heur h where 
    applyHeur :: (RandomGen gen) => h -> Board -> gen -> (Pos, h, gen)
    initHeur :: (RandomGen gen) => h -> Board -> gen -> (h, gen) 

dispatchHeurs :: (RandomGen gen, Heur heur) => [heur] -> Board -> gen -> (Pos, [heur], gen)
dispatchHeurs [] _ _ = error "no heuristic available"
dispatchHeurs (h:hs) board gen = 
    let (pos, newHeur, newGen) = applyHeur h board gen 
    in (pos, (newHeur:hs), newGen)

data HeurEmpty = HeurEmpty {heEmpty :: [Pos]} 
instance Heur HeurEmpty where
    applyHeur HeurEmpty{heEmpty=[]} board gen = error "no where to play - empty list"
    applyHeur HeurEmpty{heEmpty=xs} board gen = 
        let pos2peg = (\pos -> mkPeg (getRow pos) (getCol pos) (bdToPlay board))
            (trash, rest) = break (\pos -> isLegalMove board (pos2peg pos)) xs
        in 
            let oppCanPlay = filter (isEmptySquare board) trash 
            in
            if length(rest) == 0 then 
               ((0, 0), HeurEmpty{heEmpty=(oppCanPlay ++ rest)}, gen)
            else  
               (rest !! 0, HeurEmpty{heEmpty=(oppCanPlay ++ rest)}, gen)

    --empty squares are randomly shuffled
    initHeur _ board gen = 
        let emptyPos = getAllEmptyPos board
            (shuffled, newGen) = runState (shuffle emptyPos) gen
            heur = HeurEmpty{heEmpty=shuffled}
        in (heur, newGen)
            
-- ==============================
-- Generic playouts and simplePlayout
-- ==============================

--runs given number of playouts and returns statistics
doPlayouts :: (RandomGen gen) => Board -> gen -> Int -> [Maybe Color] 
doPlayouts board gen n = 
    let 
        games = evalState (sequence $ replicate n (doPlayout board)) gen
    in map gmWinner games

--generic playout dispatcher
doPlayout :: (RandomGen gen) => Board -> State gen Game
doPlayout board = 
    do 
    gen <- get
    let (h, newGen) = initHeur HeurEmpty{heEmpty=[]} board gen 
    put newGen
    game <- simplePlayout board [h]
    return game

--simplePlayout with heuristics 
simplePlayout :: (RandomGen gen, Heur heur) => Board -> [heur] -> State gen Game 
simplePlayout board heurs | hasWinner board = return $ mkGame (getWinner board) []
simplePlayout board heurs | isDraw board = return $ mkGame Nothing []
simplePlayout board heurs | otherwise = 
    do 
        gen <- get
        let (pos, newHeurs, newGen) = dispatchHeurs heurs board gen
        let peg = mkPeg (getRow pos) (getCol pos) (bdToPlay board) 
        let newBoard = makeMove board peg 
        game <- simplePlayout newBoard newHeurs
        {- t1142c
        trace (show board)
        -}
        return $ mkGame (gmWinner game) (peg:(gmMoves game))

makeMove :: Board -> Move -> Board
makeMove board peg | pegPos peg == (0, 0) = board {bdToPlay = oppColor $ bdToPlay board}
makeMove board peg | otherwise = placePeg board peg

