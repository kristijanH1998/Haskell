import Prelude hiding (replicate)
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
