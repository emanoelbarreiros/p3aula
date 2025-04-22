module Merge (mergesort) where

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort l = merge (mergesort left) (mergesort right)
              where
                  mid = div (length l) 2
                  left = take mid l
                  right = drop mid l

merge :: Ord a => [a] -> [a] -> [a]
merge [] r = r 
merge l [] = l
merge (l:ls) (r:rs)
    | l < r = l : merge ls (r:rs)
    | otherwise = r : merge (l:ls) rs