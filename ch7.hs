import Prelude hiding (map, filter)

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

main = do
    print $ map (+1) $ filter even [1..10]
    print $ mapFilt (+1) even [1..10]