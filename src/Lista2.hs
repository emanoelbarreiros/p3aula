module Lista2 where

--questao 1
soma :: Int
soma = sum [x*x | x <- [1..100]]

--questao 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

--questao 3
quadrado :: Int -> [(Int, Int)]
quadrado l = [(x,y) | (x,y) <- grid l l, x /= y]

--questao 4
meuReplicate :: (Num t, Enum t) => t -> a -> [a]
meuReplicate n v = [v | _ <- [1 .. n]]

--questao 5
pitag :: Int -> [(Int, Int, Int)]
pitag n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]

--questao 6
fatores' :: Integral a => a -> [a]
fatores' x = [n | n <- [1..(div x 2)], mod x n == 0]

perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1..n], sum (fatores' x) == x]

--questao 7
compreensao :: [(Int, Int)]
compreensao = concat [[(1,y) | y <- [3,4]], [(2,y) | y <- [3,4]]]

--questao 8
buscar :: Eq a => a -> [(a,b)] -> [b]
buscar k xs = [v | (k', v) <- xs, k == k']

posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = [i | (z, i) <- zip xs [0 ..], x == z]

posicoes' :: Eq a => a -> [a] -> [Int]
posicoes' x xs = buscar x (zip xs [0 ..])

--questao 9
produtoEscalar :: Num a => [a] -> [a] -> a
produtoEscalar xs ys = sum [x * y | (x,y) <- zip xs ys]