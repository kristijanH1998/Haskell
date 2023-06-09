
--5.2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]
--5.3
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]
main = do
    --5.1
    print $ [sum([x^2 | x <- [1..100]])]
    print $ grid 1 2
    print $ square 2
    