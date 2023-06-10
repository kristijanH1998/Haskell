import Prelude hiding (replicate)
import Data.Char
--5.2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]
--5.3
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]
--5.4
replicate :: Int -> a -> [a]
replicate a b = [b | _ <- [1..a]]
--5.5
pyths :: Int -> [(Int,Int,Int)]
pyths x = [(a,b,c) | a <- [1..x], b <- [1..x], c <-[1..x], (a^2) + (b^2) == (c^2)]
--5.6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
perfects :: Int -> [Int]
perfects x = [n | n <- [1..x], n == sum(tail(reverse(factors n)))]
--5.8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

mypositions :: Eq a => a -> [a] -> [Int]
mypositions x xs = find x [(y,z) | (y,z) <- zip xs [0..]]
--5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]
--5.10
let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
          | otherwise = ord c - ord 'A'
int2letL :: Int -> Char
int2letL n = chr (ord 'a' + n)
int2letU :: Int -> Char
int2letU n = chr (ord 'A' + n)
shift :: Int -> Char -> Char
shift n c | isLower c = int2letL ((let2int c + n) `mod` 26)
          | isUpper c = int2letU ((let2int c + n) `mod` 26)
          | otherwise = c
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
table :: [Float]
table = [8.1,1.5,2.8,4.2,12.7,2.2,2.0,6.1,7.0,
        0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,
        6.3,9.0,2.8,1.0,2.4,0.2,2.0,0.1]
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100
letters :: String -> Int
letters xs = length [x | x <- xs, isAlpha x]
freqs :: String -> [Float]
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']
freqs xs = [percent (count x xs') n | x <- ['a'..'z']]
           where xs' = map toLower xs
                 n = letters xs  
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e) ^ 2) / e | (o,e) <- zip os es]
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs
crack :: String -> String
crack xs = encode (-factor) xs
    where
        factor = head(positions(minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs
--
main = do
    --5.1
    print $ [sum([x^2 | x <- [1..100]])]
    --
    print $ grid 1 2
    print $ square 2
    print $ replicate 3 True
    --print $ pyths 50
    --print $ perfects 10000
    --5.7
    print $ [(x,y) | x <- [1,2], y <- [3,4]]
    print $ concat [[(1,y) | y <- [3,4]], [(2,z) | z <- [3,4]]]
    print $ concat [[(x,y) | y <- [3,4]] | x <- [1,2]]
    --5.8
    print $ positions False [True, False, True, False]
    print $ find 'b' [('a',1), ('b',2),('c',3),('b',4)]
    print $ mypositions False [True, False, True, False]
    print $ mypositions 2 [1,1,3,4,5,2,2,8,2]
    --5.9
    print $ scalarproduct [2,3,4] [5,6,7]
    --5.10
    print $ shift 3 'a'
    print $ shift 3 'A'
    print $ encode 3 "haskell is fun"
    print $ crack "kdvnhoo lv ixq"
    print $ crack "vscd mywzboroxcsyxc kbo ecopev"
    print $ crack (encode 3 "my name is Kristijan")