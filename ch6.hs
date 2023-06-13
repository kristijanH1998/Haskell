import Prelude hiding ((^))
--6.1
--if applied to negative numbers, recursion of fac never terminates because its negative argument is repeatedly decreased by 1 
--and never reaches 0
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
