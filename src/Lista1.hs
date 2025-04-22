module Lista1 where

import Lista2
import Text.ParserCombinators.ReadP (sepBy)

--questao 1
iguais :: (Eq a, Num b) => a -> a -> a -> b
iguais a b c
    | a == b && a == c = 3
    | a /= b && a /= c && b /= c = 0
    | otherwise = 2

--questao 2
maiorQueMedia :: (Ord a, Fractional a, Num b) => a -> a -> a -> b
maiorQueMedia a b c = maior' media a + maior' media b + maior' media c
                      where
                        media = (a + b + c) / 3
                        maior' v n = if n > v then 1 else 0

--questao 3
potencia_2 :: Num a => a -> a
potencia_2 x = x * x

--questao 4
potencia_4 :: Int -> Int
potencia_4 = potencia_2 . potencia_2

--questao 5
xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not (x && y)

--questao 6
delta :: Num a => a -> a -> a -> a
delta a b c = b * b - 4 * a * c

xMaior :: (Ord a, Floating a) => a -> a -> a -> Maybe a
xMaior a b c
    | d < 0 = Nothing
    | otherwise = Just $ (negate b + sqrt d) / (2 * a)
    where
        d = delta a b c

xMenor :: (Ord a, Floating a) => a -> a -> a -> Maybe a
xMenor a b c
    | d < 0 = Nothing
    | otherwise = Just $ (negate b - sqrt d) / (2 * a)
    where
        d = delta a b c

--questao 7
somaIntervalo :: (Num a, Enum a, Ord a) => a -> a -> a
somaIntervalo n1 n2 = sum [x .. y]
                      where
                        x = min n1 n2
                        y = max n1 n2

somaIntervalo' :: (Num a, Enum a, Ord a) => a -> a -> a
somaIntervalo' n1 n2 = sum [(x+1) .. (y-1)]
                       where
                        x = min n1 n2
                        y = max n1 n2

--questao 8
-- A solucao mais direta é usando compreensao de listas,
-- mas como nessa lista eu ainda não tinha ensinado,
-- resolvi sem a compreensão.
multiplusIntervalo :: Integral a => a -> a -> a -> [Bool]
multiplusIntervalo n1 n2 n3 = mapear (\z -> mod z n3 == 0) [x .. y]
                              where
                                x = min n1 n2
                                y = max n1 n2

-- essa seria a funcao map do prelude
mapear :: (t -> a) -> [t] -> [a]
mapear _ [] = []
mapear f (x:xs) = f x : mapear f xs

--questao 9
multiplica :: Int -> Int -> Int
multiplica n1 n2
    | signum n1 /= signum n2 = negate res
    | otherwise = res
    where
        x = abs n1
        y = abs n2
        res = sum (replicate x y)

--questao 10
mod2 :: Integral a => a -> a -> a
mod2 x y
    | x > y = mod (x-y) y
    | otherwise = x

--questao 11
sequencia :: (Eq t, Floating a, Num t) => t -> a
sequencia n
    | n == 1 = sqrt 6
    | otherwise = sqrt (6 + sequencia (n - 1))

--questao 12
fatorial :: (Num a, Enum a) => a -> a
fatorial n = product [1 .. n]

combinacao :: (Num a, Enum a) => a -> a -> a
combinacao n p = fatorial p * fatorial (n - p)

--questao 13
maior :: (Ord t, Num b, Enum b) => [t] -> Maybe (t, b)
maior [] = Nothing
maior l = buscar' (maximum l) (zip l [0 ..])

buscar' :: Eq t => t -> [(t, b)] -> Maybe (t, b)
buscar' _ [] = Nothing
buscar' v (t@(x,_):xs) = if v == x then Just t else buscar' v xs

--questao 14
dict10 :: [(Int, String)]
dict10 = [(0,"zero"),(1,"um"),(2,"dois"),(3,"três"),(4,"quatro"),(5,"cinco"),(6,"seis"),(7,"sete"),(8,"oito"),(9,"nove")]

converter :: [Int] -> [String]
converter [] = []
converter (x:xs) = buscar x dict10 ++ converter xs

--questao 15
delPosicaoN :: [Int] -> Int -> [Int]
delPosicaoN [] _ = []
delPosicaoN (x:xs) n
    | n < 0 = error "indice negativo"
    | otherwise = if n == 0 then xs else x : delPosicaoN xs (n-1)

--questao 16
inserirPosicaoX :: [Int] -> Int -> Int -> [Int]
inserirPosicaoX [] n _ = [n]
inserirPosicaoX l n 0 = n : l
inserirPosicaoX (x:xs) n p = x : inserirPosicaoX xs n (p - 1)

--questao 17
posicao :: (Eq t, Num t) => t -> [a] -> a
posicao _ [] = error "indice fora do intervalo"
posicao 0 (x:_) = x
posicao n (_:xs) = posicao (n-1) xs

--questao 18
merge' :: Ord a => [a] -> [a] -> [a]
merge' [] r = r 
merge' l [] = l
merge' (l:ls) (r:rs)
    | l < r = l : merge' ls (r:rs)
    | otherwise = r : merge' (l:ls) rs

--questao 19
unir :: Eq a => [a] -> [a] -> [a]
unir [] _ = []
unir _ [] = []
unir (x:xs) l = if elem x l then x : unir xs l else unir xs l

--questao 20
empacotar' :: Eq a => [a] -> [[a]]
empacotar' [] = []
empacotar' l = aux2' l []

aux2' :: Eq a => [a] -> [[a]] -> [[a]]
aux2' [] r = r
aux2' (x:xs) r = if not (null r) && elem x (last r)
                then aux2' xs (init r ++ [x:last r])
                else aux2' xs (r ++ [[x]])

comprimir' l = concat $ map trans (empacotar' l)

trans l = if length l > 1 
          then "!" ++ show (length l) ++ [head l] 
          else l

--questao 21
descomprimir :: String -> String
descomprimir [] = []
descomprimir l = concatMap extender (sep l)

sep :: String -> [String]
sep [] = []
sep (x1:x2:x3:xs) = if x1 == '!'
                    then [x1,x2,x3] : sep xs
                    else [x1] : sep (x2:x3:xs)
sep (x:xs) = [x] : sep xs

extender :: String -> String
extender s = if length s == 1 
             then s
             else replicate (read [s!!1] :: Int) (last s)
