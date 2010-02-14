module Utils
where

import qualified Data.Map as Map

slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice n xs =  head_:(slice n tail_)
              where (head_, tail_) = splitAt n xs

exportMaybes :: [Maybe a] -> [a]
exportMaybes [] = []
exportMaybes (Just x:xs) = x:(exportMaybes xs)
exportMaybes (Nothing:xs) = exportMaybes xs

swapPair :: (a,b) -> (b, a)
swapPair (a, b) = (b, a)

mapFetch :: Ord k => [k] -> Map.Map k a -> [a]
mapFetch keys oldMap = exportMaybes $ map (\key -> Map.lookup key oldMap) keys  
