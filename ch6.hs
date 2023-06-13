import Prelude hiding ((^))
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
    
