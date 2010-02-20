
module Playout where 

import Random
import Debug.Trace
import Control.Monad.State

import Board

simplePlayout :: (RandomGen gen) => Board -> gen -> (Maybe Color, gen)
simplePlayout board gen | hasWinner board = (getWinner board, gen)
simplePlayout board gen | isDraw board = (Nothing, gen)
simplePlayout board gen | otherwise = 
    let playable = getPlayablePos board (bdToPlay board)
        (index, newGen) = randomR (0, (length playable - 1)) gen 
        pos = playable !! index
        peg = mkPeg (getRow pos) (getCol pos) (bdToPlay board) 
        newBoard = placePeg board peg
    in 
        {-
        trace ("===========")
        trace (show (bdPegMap board))
        trace (show board ++ (show index) ++ (show playable))
        trace (show peg)
        trace (show newBoard)
        -}
        simplePlayout newBoard newGen

pmSimple :: (RandomGen g) => Board -> State g (Maybe Color)
pmSimple board = 
    do 
        gen <- get  
        let (result, newGen) = simplePlayout board gen 
        put newGen
        return result 

--newtype Playout = (RandomGen g) => State g (Maybe Color)

--runs given number of playouts and returns statistics
doPlayouts :: (RandomGen gen) => Board -> gen -> Int -> [Maybe Color] -- (Int, Int)
doPlayouts board gen n = 
    let 
        results = evalState (sequence $ replicate n (pmSimple board))
    in results gen


