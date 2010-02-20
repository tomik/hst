
import Random

import Board
import Playout

main = let results = doPlayouts (mkBoard 11 11) (mkStdGen 10) 50 
       in do 
       putStrLn $ show results
        
