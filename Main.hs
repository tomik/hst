
import Random

import Control.Monad.State
import Board
import Playout

main = 
       let results = doPlayouts (mkBoard 14 14) (mkStdGen 27) 100
       --let results = evalState (doPlayout (mkBoard 11 11)) (mkStdGen 18)
       in do 
       putStrLn $ show results
       --putStrLn $ pegsToJsonStr $ gmMoves results
        

