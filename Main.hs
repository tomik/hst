
import Random

import Control.Monad.State
import Board
import Playout

main = 
       let results = doPlayouts (mkBoard 24 24) (mkStdGen 12) 10
       --let results = evalState (doPlayout (mkBoard 11 11)) (mkStdGen 8)
       in do 
       putStrLn $ show results
       --putStrLn $ pegsToJsonStr $ gmMoves results
        

