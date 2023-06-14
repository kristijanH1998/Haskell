import Prelude hiding ((^), and, concat, replicate, (!!), elem)
--6.1
-- if applied to negative numbers, recursion of fac never terminates because its negative argument is repeatedly decreased by 1 
-- and never reaches 0
fac :: Int -> Int
fac 0 = 1
fac n | n > 0   = n * fac (n-1)
      | otherwise = -1
--6.2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0   = n + sumdown (n-1)
          | otherwise = -1
--6.3
(^) :: Int -> Int -> Int
a ^ 1 = a
a ^ b = a * (a ^ (b-1))
{-
    6.3 execution for 2 ^ 3: 2 ^ 3 {applying ^} = 2 * (2 ^ 2) {applying ^} = 2 * (2 * (2 ^ 1)) {applying ^} = 2 * (2 * (2)) {applying *}
    = 8
-}
--6.4
euclid :: Int -> Int -> Int
euclid a b | (a < 0) || (b < 0)     = -1
           | (a == 0)               = b
           | (b == 0)               = a
           | a == b                 = b 
           | otherwise              = if (a < b) then euclid a (b - a) else euclid (a - b) b
--6.6 a)
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and(xs)
--6.6 b)
concat :: [[a]] -> [a]
concat [] = []
concat (xs : xss) = xs ++ concat xss
--6.6 c)
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n e = [e] ++ replicate (n-1) e
--6.6 d)
(!!) :: [a] -> Int -> a
(!!) [] x = error "Error: The list is empty."
(!!) (n:ns) 0 = n
(!!) (n:ns) x = if x < 0 || x > (length (n:ns) - 1) then error "Error: index out of bounds." 
                                                    else (!!) ns (x-1)
--6.6 e)
elem :: Eq a => a -> [a] -> Bool
elem x [] = False
elem x (n:ns) = if x == n then True else elem x ns
--6.7
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] (x:xs) = [x] ++ merge [] xs
merge (x:xs) [] = [x] ++ merge xs []
merge (x:xs) (n:ns) = if x >= n then [n] ++ merge (x:xs) ns else [x] ++ merge xs (n:ns)
--6.8
halve :: [a] -> ([a],[a])
halve (x:xs) = splitAt (length (x:xs) `div` 2) (x:xs) 
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort [x,y] = if x <= y then [x,y] else [y,x]
msort (x:xs) = merge (msort (fst (halve (x:xs)))) (msort (snd (halve (x:xs))))

main = do
    print $ fac(10)
    print $ sumdown(-2)
    print $ 2 ^ 3
    print $ euclid 28 6
    {- 6.5
    length [1,2,3] evaluation = {applying length} 1 + length [2,3] = {applying length} 1 + (1 + length [3]) = {applying length}
    1 + (1 + (1 + length [])) = {applying length} 1 + (1 + (1 + 0)) = {applying +} 3
    drop 3 [1,2,3,4,5] evaluation = {applying drop} drop 2 [2,3,4,5] = {applying drop} drop 1 [3,4,5] = {applying drop} drop 0 [4,5]
    = {applying drop} [4,5]
    init [1,2,3] evaluation = {applying init} 1 : init[2,3] = {applying init} 1 : 2 : init [3] = {applying init} 1 : 2 : [] = 
    {list notation} [1,2]
    -}
    print $ and ([True,True,True,True])
    print $ concat [[7,8,9], [1,2,3], [4,5,6]]
    print $ replicate 10 "word"
    print $ (!!) (['a','b','c','d']) 2
    print $ elem False [False, False, False]
    print $ merge [4,7,18,235] [1,3,4]
    print $ msort [64,1,44,12,78,3,4,4,66]