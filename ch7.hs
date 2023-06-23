import Prelude hiding (map, filter, all, any, takeWhile, dropWhile, curry)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]
--7.1
mapFilt :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilt f p [] = []
--[f x | x <- xs, p x] can be expressed with map and filter as:
mapFilt f p (xs) = map f (filter p xs)
--7.2
--a)
all :: (a -> Bool) -> [a] -> Bool
all p [] = True
all p (x:xs) = if p x then all p xs else False
all2 :: (a -> Bool) -> [a] -> Bool
all2 p xs = and (map p xs)
--b)
any :: (a -> Bool) -> [a] -> Bool
any p [] = False
any p (x:xs) = if p x then True else any p xs
any2 :: (a -> Bool) -> [a] -> Bool
any2 p xs = or (map p xs)
--c)
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs) | p x = x : takeWhile p xs
                   | otherwise = takeWhile p [] 
--d)
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs) = if p x then dropWhile p xs else x:xs
--7.3
mapFldr f = foldr (\x xs -> f x : xs) []
filterFldr p = foldr (\x xs -> if p x then x:xs else xs) []
--7.4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> y + 10*x) 0
--7.5
curry :: ((a, b) -> c) -> (a -> (b -> c))
curry f = \x y -> f(x,y)  
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(x,y) -> f x y

main = do
    print $ map (+1) $ filter even [1..10]
    print $ mapFilt (+1) even [1..10]
    print $ all odd [1,3,5,7,8]
    print $ any odd [2,4,6,8,9]
    print $ all2 odd [1,3,5,7,8]
    print $ any2 odd [2,4,6,8,9]
    print $ takeWhile (<= 4) [1,2,3,4,5,6,7]
    print $ dropWhile (<= 4) [1,2,3,4,5,6,7]
    print $ dropWhile (even) [4,6,8,9,11,15]
    print $ mapFldr (*2) [1,2,3,4,5]
    print $ filterFldr (<10) [11,21,2,4,15,22,1] 
    print $ dec2int [7,5,2,7]