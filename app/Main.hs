module Main (main) where

import Data.List
import Lib


main :: IO ()
main = do
    someFunc


somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort (fst particao) ++ [x] ++ quicksort (snd particao)
                   where
                      particao = partition (< x) xs

myButLast :: [a] -> a
myButLast n = head $ drop (length n - 2) n

tamanho :: [a] -> Int
tamanho [] = 0
tamanho (_:xs) = 1 + tamanho xs

inverter [] = []
inverter l = last l : inverter (init l)

ehPalindromo :: Eq a => [a] -> Bool
ehPalindromo l = l == lInvertido
                 where
                     lInvertido = inverter l

comprimir :: Eq a => [a] -> [a]
comprimir [] = []
comprimir l = aux l []

aux :: Eq a => [a] -> [a] -> [a]
aux [] r = r
aux (x:xs) r = if not (null r) && (last r == x)
               then aux xs r
               else aux xs (r ++ [x])

empacotar :: Eq a => [a] -> [[a]]
empacotar [] = []
empacotar l = aux2 l []

aux2 :: Eq a => [a] -> [[a]] -> [[a]]
aux2 [] r = r
aux2 (x:xs) r = if not (null r) && elem x (last r)
                then aux2 xs (init r ++ [x:last r])
                else aux2 xs (r ++ [[x]])

