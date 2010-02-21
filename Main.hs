
import Random

import Board
import Playout

main = let results = doPlayouts (mkBoard 24 24) (mkStdGen 10) 10
       in do 
       putStrLn $ show results
        
