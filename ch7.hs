import Prelude hiding (map, filter, all, any, takeWhile, dropWhile, curry, id, iterate)
import Data.Char
type Bit = Int

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]
id :: a -> a
id = \x -> x
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
--7.6
unfold p h t x | p x    = []
               | otherwise = h x : unfold p h t (t x)
chop9 :: [Bit] -> [[Bit]]
chop9 = unfold (== []) (take 9) (drop 9)
map2 :: (a -> b) -> [a] -> [b]
map2 f = unfold null (f.head) (tail)
iterate f = unfold (const False) (id) (f)
--7.7
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
               where weights = iterate ( *2) 1
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)
countOnes :: [Bit] -> Int
countOnes [] = 0
countOnes (x:xs) = if x == 1 then (1 + countOnes xs) else countOnes xs
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0) ++ (if even (countOnes bits) then [0] else [1]) 
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)
checkParity :: [Bit] -> Bool
checkParity [] = False
checkParity xs = if ((last xs == 0) && (even (countOnes (init xs)))) || ((last xs == 1) && (not (even (countOnes (init xs))))) then True else False
decode :: [Bit] -> String
decode bits = if all (== True) (map checkParity (chop9 bits)) then map (chr . bin2int) (map init (chop9 bits)) else error "Parity Error"
transmit :: String -> String
transmit = decode . channel . encode
channel :: [Bit] -> [Bit]
channel = id
--7.8
faultyChannel :: [Bit] -> [Bit]
faultyChannel bits = tail bits
faultyTransmit :: String -> String
faultyTransmit = decode . faultyChannel . encode

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
    print $ mapFldr ( *2) [1,2,3,4,5]
    print $ filterFldr (<10) [11,21,2,4,15,22,1] 
    print $ dec2int [7,5,2,7]
    print $ map2 (+2) [1,2,3]
    print $ encode "abc"
    print $ transmit "Kristijan"
    print $ countOnes [0,1,1,0,1,1,0,1]
    print $ make8 [1,1,1]
    print $ checkParity [1,0,0,0,1,0,0,1,0]
    print $ encode "Kiki"
    print $ decode $ encode "My secret code"
    print $ decode [0,1,1,1,0,1,0,0,0]
    --print $ faultyTransmit "A new message."