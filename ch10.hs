import Prelude hiding (putStr, putStrLn)
import Data.Char

putStr :: String -> IO ()
putStr []     = return ()
putStr (x:xs) = do putChar x
                   putStr xs
putStrLn :: String -> IO ()
putStrLn xs = do putStr xs
                 putChar '\n'

--Game of Nim

next :: Int -> Int
next 1 = 2
next 2 = 1
type Board = [Int]
initial :: Board
initial = [5,4,3,2,1]
finished :: Board -> Bool
finished = all (== 0)
valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num
move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n-num else n
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))
putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e
--10.2
myPutBoard (starNum:starNums) = myPutBoardAux (starNum:starNums) 1
myPutBoardAux :: Board -> Int -> IO ()
myPutBoardAux [] _                   = return ()
myPutBoardAux (starNum:starNums) row = do putRow row starNum
                                          myPutBoardAux starNums (row+1)                                       
--10.3
myPutBoard2 :: Board -> IO ()
myPutBoard2 board = sequence_ [putRow row stars | (row,stars) <- zip [1..] board]

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERROR: Invalid digit"
                           getDigit prompt
newline :: IO ()
newline = putChar '\n'
play :: Board -> Int -> IO ()
play board player = 
    do newline
       putBoard board
       if finished board then
            do newline
               putStr "Player "
               putStr (show (next player))
               putStrLn " wins!!"
       else
            do newline
               putStr "Player "
               putStrLn (show player)
               row <- getDigit "Enter a row number: "
               num <- getDigit "Stars to remove: "
               if (valid board row num) then
                    play (move board row num) (next player)
               else
                    do newline
                       putStrLn "ERROR: Invalid move"
                       play board player
nim :: IO ()
nim = play initial 1
--10.1
putStr1 :: String -> IO ()
putStr1 [] = return ()
putStr1 str = sequence_ [putChar c | c <- str]
--10.4
adder :: IO ()
adder = do putStrLn "How many numbers? "
           nums <- getLine
           if nums /= "0" then 
               do
                  putStr "The total is " 
                  adderAux 0 (read nums :: Int) 
           else
               putStrLn "0"
adderAux :: Int -> Int -> IO ()
adderAux curTotal 0 = do putStrLn $ (show curTotal)         
adderAux curTotal numsRem = do num <- getLine
                               adderAux (curTotal + (read num :: Int)) (numsRem - 1)
--10.5
adder' :: IO ()
adder' = do putStrLn "How many numbers? "
            n <- getLine
            nums <- sequence [getLine | _ <- [1..(read n :: Int)]]
            putStrLn $ "The total is " ++ show (sum [(read num :: Int) | num <- nums])
--10.6
readLine :: IO String
readLine = do ch <- getChar
              if ch == '\n' then
                  return ""
              else
                  do chs <- getLine 
                     return (ch:chs)


main = do
    putStr1 ("This is a sentence.\n")
    putBoard initial 
    --nim
    myPutBoard [10,9,8,7,6,5,4,3,2,1]
    myPutBoard2 [10,9,8,7,6,5,4,3,2,1]
    --adder
    --adder'
    str <- readLine
    print str
