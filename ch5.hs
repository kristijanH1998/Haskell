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
main = do
    --5.1
    print $ [sum([x^2 | x <- [1..100]])]
    --
    print $ grid 1 2
    print $ square 2
    print $ replicate 3 True