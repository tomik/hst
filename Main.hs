
import Random

import Control.Monad.State
import Board
import Playout

--results to W/B/Draw triple
makeStats :: [Maybe Color] -> (Int, Int, Int)  
makeStats results = 
    let whiteWins = length $ filter (\r -> r == Just White) results
        blackWins = length $ filter (\r -> r == Just Black) results
        draws = length $ filter (\r -> r == Nothing) results
    in (whiteWins, blackWins, draws)

main = 
       let results = doPlayouts (mkBoard 24 24) (mkStdGen 27) 100
       --let results = evalState (doPlayout (mkBoard 11 11)) (mkStdGen 18)
       in do 
       putStrLn $ show $ makeStats results
       --putStrLn $ pegsToJsonStr $ gmMoves results
        

