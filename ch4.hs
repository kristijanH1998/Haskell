import Prelude hiding (null, (||))
--4.1
halve :: [a] -> ([a],[a])
halve (xs) = splitAt ((length xs) `div` 2) xs

--4.2 
--a) 
third1 :: Num a => [a] -> a
third1 (xs) = if (length xs) >= 3 then (head (tail (tail xs))) else (-1)         --if list contains less than 3 elements, prints -1
--b)
third2 :: Num a => [a] -> a
third2 (xs) | length xs >= 3    = xs !! 2
            | otherwise         = (-1)
--c)
third3 :: Num a => [a] -> a
third3  (_:_:a:_) = a
third3  _          = (-1)

--4.3
null :: [a] -> Bool
null (a) = if length a == 0 then True else False
--a) a conditional expression
safetail1 :: [x] -> [x]
safetail1 xs = if null xs then [] else tail xs
--b) guarded equations
safetail2 :: [x] -> [x]
safetail2 xs | null xs  = []
             | otherwise = tail xs
--c) pattern matching
safetail3 :: [x] -> [x]
safetail3 [] = []
safetail3 (xs)  = tail xs

--4.4
(||) :: Bool -> Bool -> Bool
--option1:
True || True    = True
True || False   = True
False || True   = True
False || False  = False
{- option2: 
False||False= False
_ || _   = True

option3:
True || _   = True
False||b = b

option4:
b || c | b == c    = b
       | otherwise = True
-}
main = do
    putStrLn $ show (halve[2,4..8])
    putStrLn $ show (third1[1,2,3,4,5])
    putStrLn $ show (third1[1,2])
    putStrLn $ show (third2[1,2,3,4,5])
    putStrLn $ show (third2[1,2])
    putStrLn $ show (third3[3,6..18])
    putStrLn $ show (null[])
    putStrLn $ show (safetail1([] :: [Int]))
    putStrLn $ show (safetail2([] :: [Int]))
    putStrLn $ show (safetail3([1,2,3,4,5] :: [Int]))
    putStrLn $ show ((||) False False)




