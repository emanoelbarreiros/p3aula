module Lista3 where

--questao 1
fatorial :: (Num t, Ord t) => t -> t
fatorial 0 = 0
fatorial n
    | n < 0 = error "impossível calcular o fatorial pra numero negativo"
    | otherwise = n * fatorial (n - 1)

--questao 2
somar :: (Eq t, Num t) => t -> t
somar 0 = 0
somar n = n + somar (n - 1)


--questao 3
(^^^) :: Int -> Int -> Int
_ ^^^ 0 = 1
m ^^^ n = m * (m ^^^ (n - 1)) 


--questao 4
euclides m n
    | m == n = m
    | otherwise = euclides (maior - menor) menor
    where
        menor = min m n
        maior = max m n

--questao 5
--a
and2 [] = True
and2 (x:xs) = x && and2 xs

--b
concat2 [] = []
concat2 (x:xs) = x ++ concat2 xs

concat3 :: Foldable t => t [a] -> [a]
concat3 = foldr (++) []

--c
replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 n el = el : replicate2 (n - 1) el

--d
(!!!) :: [a] -> Int -> a
[] !!! _ = error "indice fora do intervalo"
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)

--e
elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False
elem2 v (x:xs) = x == v || elem2 v xs

--questao 6
merge :: Ord a => [a] -> [a] -> [a]
merge [] r = r 
merge l [] = l
merge (l:ls) (r:rs)
    | l < r = l : merge ls (r:rs)
    | otherwise = r : merge (l:ls) rs

--questao 6
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort l = merge (mergesort left) (mergesort right)
              where
                  mid = div (length l) 2
                  left = take mid l
                  right = drop mid l

--questao 8
--a
soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

--b
length2 :: [a] -> Int
length2 [] = 0
length2 (_:xs) = 1 + length xs

--c
ultimo :: [a] -> a
ultimo [] = error "lista vazia"
ultimo [x] = x
ultimo (_:xs) = ultimo xs