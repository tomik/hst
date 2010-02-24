
module Playout where 

import Random
import Debug.Trace
import Control.Monad.State
import Data.List

import Board

type Move = Peg
type Moves = Pegs
data Game = Game {gmWinner :: Maybe Color, gmMoves :: Moves} deriving (Show, Eq)

mkGame winner moves = Game { gmWinner = winner, gmMoves = moves} 

simplePlayout :: (RandomGen gen) => Board -> State gen Game 
simplePlayout board | hasWinner board = return $ mkGame (getWinner board) []
simplePlayout board | isDraw board = return $ mkGame Nothing []
simplePlayout board | otherwise = 
    do 
        let playable = bdCanPlay board (bdToPlay board)
        gen <- get
        let (index, newGen) = randomR (0, (length playable - 1)) gen 
        put newGen
        let pos = playable !! index
        let peg = mkPeg (getRow pos) (getCol pos) (bdToPlay board) 
        let newBoard = placePeg board peg
        game <- simplePlayout newBoard
        return $ mkGame (gmWinner game) (peg:(gmMoves game))
        
        {-
        trace ("===========")
        trace (show (bdPegMap board))
        trace (show board ++ (show index) ++ (show playable))
        trace (show peg)
        trace (show newBoard)
        -}

--runs given number of playouts and returns statistics
doPlayouts :: (RandomGen gen) => Board -> gen -> Int -> [Maybe Color] -- (Int, Int)
doPlayouts board gen n = 
    let 
        games = evalState (sequence $ replicate n (simplePlayout board)) gen
    in map gmWinner games

