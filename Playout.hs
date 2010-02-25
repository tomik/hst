
module Playout where

import Random
import Debug.Trace
import Control.Monad.State
import System.Random.Shuffle
import Data.List

import Board

data Move = Pass | Peg
type Moves = Pegs
data Game = Game {gmWinner :: Maybe Color, gmMoves :: Moves} deriving (Show, Eq)

mkGame winner moves = Game {gmWinner = winner, gmMoves = moves} 

-- ==============================
-- Heuristics
-- ==============================

class Heur h where 
    applyHeur :: (RandomGen gen) => h -> Board -> gen -> (Pos, h, gen)
    initHeur :: (RandomGen gen) => h -> Board -> gen -> (h, gen) 

dispatchHeurs :: (RandomGen gen, Heur heur) => [heur] -> Board -> gen -> (Pos, [heur], gen)
dispatchHeurs [] _ _ = error "no heuristic available"
dispatchHeurs (h:hs) board gen = 
    let (pos, newHeur, newGen) = applyHeur h board gen in 
    (pos, (newHeur:hs), newGen)

data HeurEmpty = HeurEmpty {heEmpty :: [Pos]} 
instance Heur HeurEmpty where
    applyHeur HeurEmpty{heEmpty=[]} board gen = error "no where to play - empty list"
    applyHeur HeurEmpty{heEmpty=xs} board gen = 
        let pos2peg = (\pos -> mkPeg (getRow pos) (getCol pos) (bdToPlay board))
            (trash, rest) = break (\pos -> isLegalMove board (pos2peg pos)) xs
        in 
            let oppCanPlay = filter (isEmptySquare board) trash 
            in (rest !! 0, HeurEmpty{heEmpty=(oppCanPlay ++ rest)}, gen)

    --empty squares are randomly shuffled
    initHeur _ board gen = 
        let playable = getAllPlayablePos board
            heur = HeurEmpty{heEmpty=(shuffle' playable (length playable) gen)}
        in (heur, gen)
            
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
        let newBoard = placePeg board peg
        game <- simplePlayout newBoard newHeurs
        {- t1142c
        trace (show board)
        -}
        return $ mkGame (gmWinner game) (peg:(gmMoves game))

