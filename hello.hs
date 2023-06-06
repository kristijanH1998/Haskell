import Prelude hiding (product)
double x = x + x
triple x = 3 * x
copy a = (a,a)

--1.3
product [] = 1
product (n:ns) = n * product ns 

--1.4
qsort [] = []
qsort(x:xs) = qsort larger ++ [x] ++ qsort smaller
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [a | a <- xs, a > x]

--1.5
qsort2 [] = []
qsort2(x:xs) = qsort2 smaller ++ [x] ++ qsort2 larger
                where
                    smaller = [a | a <- xs, a < x]
                    larger = [a | a <- xs, a > x]

--2.4
last1 [] = 0
last1 (x) = head(reverse x)

last2 [] = 0
last2 [x] = x
last2 (x:xs) = last2 xs  

last3 [] = 0
last3 (x) = x !! ((length (x)) - 1)

--2.5
init1 [] = []
init1 (x) = reverse(tail(reverse x))

init2 [] = []
init2 (x) = take (length x - 1) x

init3 [] = []
init3 (x) = reverse (drop 1 (reverse x))

main = do
    --putStrLn "Hello, everybody!"
    --putStrLn $ show (take 10 (filter even [1..]))
    --putStrLn $ show ([x | x <-[1..10], odd x, x <= 8])
    
    --Exercises CH1:
    --1.1 
    putStrLn $ show (double (double 2))
    --another possible calculation of double (double 2):
    --double(double 2) = 
    --{applying the outer double}
    --double (2) + double (2)
    --applying the first double
    --(2 + 2) + double (2)
    --{applying the second double}
    --(2+2) + (2+2)
    --applying the first +
    --4 + (2 + 2)
    --applying the second +
    --4 + 4
    --applying +
    --8
    putStrLn $ show (double(double(double 2)))
    putStrLn $ show (triple(5))

    --1.2
    --sum[x] = x for any number x (Prove)
    --sum[x] = {applying sum} x + sum[] = {applying sum} x + 0 = {applying +} x

    putStrLn $ show (sum[3,4,5])
    putStrLn $ show (sum[x| x <- [1..10], even x])
    putStrLn $ show (product[2,3,4])
    putStrLn $ show (product[sum[1,2], sum[2,3]])

    --1.3
    putStrLn $ show (product[2,3,4])

    --1.4
    putStrLn $ show (qsort[12,3,5,2,2,17])

    --1.5
    --if <= was replaced by < in the original definition of qsort, for each element x the algorithm would ignore other elements in
    --x'es list that are of the same value, and the resulting list/sublist would be missing those elements. For example:
    --qsort2[2,2,3,1,1] would output [1,2,3] 

    putStrLn $ show (qsort2[2,2,3,1,1]) 
    
    --2.4
    putStrLn $ show(last1[1,2,3,4,5])
    putStrLn $ show(last2[1,2,3,4,5,6])
    putStrLn $ show(last3[1,2,3,4])

    --2.5
    putStrLn $ show(init1[1,2,3,4,5,6,7])
    putStrLn $ show(init2[1,2,3,4,5,6,7])
    putStrLn $ show(init3[1,2,3,4,5,6,7])

    {- 3.1: 1) list of three chars; [char]
            2) 3-tuple (triple) of chars; (char, char, char)
            3) list of two tuples, each containing mixed values (bool and char); [(Bool,char)]
            4) tuple of two lists, first containing bool values and the second chars; ([Bool],[char])
            5) list of functions; [[el] -> [el]]
            
     - 3.2: 1) let bools = [false, true]
            2) let nums = [[1,2,3], [2,5,7], [3,4,5]]
            3) add x y z = x + y + z
            4) copy a = (a,a)
            5) apply f a = f + a
               f a = b

     - 3.3: 1) second :: [a] -> a
            2) swap :: (a,b) -> (b,a)
            3) pair :: x -> y -> (x,y)
            4) double :: Num -> Num
            5) palindrome :: [x] -> Bool
            6) twice :: (x -> x) -> x -> x
    -}

    putStrLn $ show (copy 2)