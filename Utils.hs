module Utils
where

import qualified Data.Map as Map

--for shuffle 
import Data.Array.ST
import Data.STRef
import Control.Monad
import Control.Monad.ST
import Control.Arrow
import System.Random


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

--shuffling method from rosettacode.org
shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle list g = runST $ do
    r <- newSTRef g
    let rand range = liftM (randomR range) (readSTRef r) >>=
            runKleisli (second (Kleisli $ writeSTRef r) >>> arr fst)
    a <- newAry (1, len) list
    forM_ [len, len - 1 .. 2] $ \n -> do
        k <- rand (1, n)
        liftM2 (,) (readArray a k) (readArray a n) >>=
           runKleisli (Kleisli (writeArray a n) *** Kleisli (writeArray a k))
    liftM2 (,) (getElems a) (readSTRef r)
  where len = length list
        newAry :: (Int, Int) -> [a] -> ST s (STArray s Int a)
        newAry = newListArray

