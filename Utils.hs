module Utils 
where 

import Data.Array

slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice n xs =  head_:(slice n tail_)
              where (head_, tail_) = splitAt n xs
                        
mkArray :: (Ix a) => (a, a) -> (a -> b) -> Array a b
mkArray bnds f = array bnds [(i, f i) | i <- range bnds]

