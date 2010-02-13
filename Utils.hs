module Utils
where

slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice n xs =  head_:(slice n tail_)
              where (head_, tail_) = splitAt n xs

exportMaybes :: [Maybe a] -> [a]
exportMaybes [] = []
exportMaybes (Just x:xs) = x:(exportMaybes xs)
exportMaybes (Nothing:xs) = exportMaybes xs
