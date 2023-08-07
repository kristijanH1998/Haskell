module Main where

import Prelude 
import Data.Char
import Data.List
import System.IO
import System.Random

winningLineSize :: Int
winningLineSize = 3

size :: Int
size = 3
type Grid = [[Player]]
data Player = O | B | X
                deriving (Eq, Ord, Show)
next :: Player -> Player
next O = X
next B = B
next X = O
empty :: Grid
empty = replicate size (replicate size B)
full :: Grid -> Bool
full = all (/= B) . concat
turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
            os = length (filter (== O) ps)
            xs = length (filter (== X) ps)
            ps = concat g
wins :: Player -> Grid -> Bool
           --11.4 b)
wins p g = any (==True) [winningLine line winningLineSize p | line <- (rows ++ cols ++ dias)] 
           where 
                rows = g
                cols = transpose g
                dias = [diag g, diag (map reverse g)]
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]
won :: Grid -> Bool
won g = wins O g || wins X g
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
          where bar = [replicate ((size * 4)-1) '-']
showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
            beside = foldr1 (zipWith (++))
            bar = replicate 3 "|"
showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]
interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B
move :: Grid -> Int -> Player -> [Grid]
move g i p = 
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs, B:ys) = splitAt i (concat g)
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)
getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else 
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt
prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "
data Tree a = Node a [Tree a]
              deriving Show
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (Main.next p) | g' <- moves g p]
moves :: Grid -> Player -> [Grid]
moves g p 
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0..((size^2)-1)]] 
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]
depth :: Int
depth = 9
--4.11 d)
minimax :: Tree Grid -> Player -> Player -> Tree (Grid,Player)
minimax (Node g []) alpha beta 
  | wins O g = Node (g,O) []
  | wins X g = Node (g,X) []
  | otherwise = Node (g,B) []
minimax (Node g ts) alpha beta
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
                  where
                    ts' = mapMinimax ts alpha beta (turn g)
                    ps = [p | Node (_,p) _ <- ts']

mapMinimax (t:ts) alpha beta Player = do let childTree = minimax t alpha beta
                                         
getPlayer :: Tree (Grid,Player) -> Player
getPlayer (Node (_,p) _) = p                                    


bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = [g' | Node (g',p') _ <- ts, p' == best]
                where
                  tree = prune depth (gametree g p)
                  Node (_,best) ts = minimax tree O X
type Pos = (Int,Int)
goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
cls :: IO ()
cls = putStr "\ESC[2J"
main' :: IO ()
main' = do hSetBuffering stdout NoBuffering
           --11.4 a)
           putStrLn "Do you wish to play as O (first) or X (second)?"
           answer <- getLine
           if answer == "O" then play empty O True else if answer == "X" then play empty X False else main'
           
play :: Grid -> Player -> Bool -> IO ()
play g p first = do cls
                    goto (1,1)
                    putGrid g
                    --11.4 a)
                    if first == True then play' g p first else play' g (Main.next p) first
--11.4 a)
playAux :: Grid -> Player -> Bool -> IO ()
playAux g p first = do cls
                       goto (1,1)
                       putGrid g
                       play' g p first    

play' :: Grid -> Player -> Bool -> IO ()
play' g p first
  | wins O g       = putStrLn "Player O wins!\n"
  | wins X g       = putStrLn "Player X wins!\n"
  | full g         = putStrLn "It's a draw!\n"
  --11.4 a)
  | first == True  = if p == O then do i <- getNat (prompt p)
                                       case move g i p of
                                         [] -> do putStrLn "ERROR: Invalid move"
                                                  play' g p first
                                         [g'] -> playAux g' (Main.next p) first
                     else           do putStr "Player X is thinking..."
                                       --11.2
                                       let listbestmoves = bestmoves g p
                                       randIndex <- randomRIO (0, length listbestmoves - 1)
                                       (playAux $! (listbestmoves !! randIndex)) (Main.next p) first 
  | first == False = if p == O then do putStr "Player O is thinking..."
                                       --11.2
                                       let listbestmoves = bestmoves g p
                                       randIndex <- randomRIO (0, length listbestmoves - 1)
                                       (playAux $! (listbestmoves !! randIndex)) (Main.next p) first
                     else           do i <- getNat (prompt p)
                                       case move g i p of
                                        [] -> do putStrLn "ERROR: Invalid move"
                                                 play' g p first
                                        [g'] -> playAux g' (Main.next p) first                  
--11.1
numOfNodesAux :: Tree Grid -> Int
numOfNodesAux (Node g childNodes) = if (length childNodes) >= 1 then (length childNodes) + (sum (map numOfNodesAux childNodes))
                                    else 0
numOfNodes :: Tree Grid -> Int
numOfNodes tree = 1 + numOfNodesAux tree
findDepth :: Tree Grid -> Int
findDepth (Node g childNodes) = if (length childNodes) >= 1 then maximum (map findDepth childNodes) + 1
                                else 0 
--11.3
play2 :: Grid -> Player -> IO ()
play2 g p = do cls
               goto (1,1)
               putGrid g
               play2 g p
play2' :: Grid -> Player -> IO ()
play2' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g   = putStrLn "It's a draw!\n"
  | p == O   = do i <- getNat (prompt p)
                  case move g i p of
                    [] -> do putStrLn "ERROR: Invalid move"
                             play2' g p
                    [g'] -> play2 g' (Main.next p)
  | p == X   = do putStr "Player X is thinking..."
                  let listbestmoves = bestmoves g p
                  {- 11.2
                  randIndex <- randomRIO (0, length listbestmoves - 1)
                  (play2 $! (listbestmoves !! randIndex)) (Main.next p) -}
                  --11.3
                  let mindepth = foldr1 min (map findDepth [gametree g (Main.next p) | g <- listbestmoves])
                  let shortestpath = getIndexOf mindepth (map findDepth [gametree g (Main.next p) | g <- listbestmoves])
                  (play2 $! (listbestmoves !! shortestpath)) (Main.next p) 

--11.3 function that finds the index of the first element in the list that is equal to the argument element it receives
getIndexOf :: Int -> [Int] -> Int
getIndexOf elem (x:xs) = case (elemIndex elem (x:xs)) of 
                            Just num -> num
                            Nothing -> 0
--11.4 b)
winningLine :: [Player] -> Int -> Player -> Bool
winningLine [] _ _ = False
winningLine _ 0 _ = True
winningLine (pl:pls) marksRem p = if p == pl then
                                    if consecutives (pl:pls) marksRem p then True else winningLine pls marksRem p
                                  else winningLine pls marksRem p  
consecutives :: [Player] -> Int -> Player -> Bool
consecutives [] _ _ = False
consecutives _ 0 _ = True
consecutives (pl:pls) marksRem p = if p == pl then consecutives pls (marksRem-1) p else False


main = do
   print $ showPlayer O
   {-
   --testing randomRIO
   number <- randomRIO (0,10) :: IO Int
   putStrLn ("Your random number is: " ++ show number)
   print $ findDepth (gametree empty O)
   --print $ [gametree g O | g <- (bestmoves empty O)]
   print $ foldr1 min (map findDepth [gametree g O | g <- (bestmoves empty O)])
   --the logic below will be used for 11.3
   print $ map findDepth [gametree g X | g <- (bestmoves [[O,B,B],[X,X,O],[X,O,B]] O)]
   
   {- testing certain functions...
   print $ findDepth (gametree [[X,O,X],[X,B,O],[O,X,B]] X)
   print $ bestmoves [[X,O,X],[X,B,O],[O,X,B]] X
   print $ gametree [[X,O,X],[X,X,O],[O,X,B]] X
   print $ gametree [[X,O,X],[X,B,O],[O,X,X]] X
   print $ length (bestmoves [[O,B,B],[X,X,O],[X,O,B]] O)
   -}
   --main'
   -}
   print $ winningLine [X,O,X,X,O] 2 O
   print $ winningLine [X,X,O,O,X] 2 O
   --testing 11.4 b)
   print $ wins O [[O,X,X],[X,X,O],[O,O,X]]
   print $ wins O [[O,O,X],[X,X,O],[O,X,X]]