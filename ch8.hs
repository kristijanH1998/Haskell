import Prelude hiding ()
import Data.Char
--8.9
data Expr = Val Int | Add Expr Expr | Mult Expr Expr
type Cont = [Op]
data Op = EVALmul Expr | EVALadd Expr | ADD Int | MULT Int
eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVALadd y : c)
eval (Mult x y) c = eval x (EVALmul y : c)
exec :: Cont -> Int -> Int
exec []             n = n
exec (EVALadd y : c)   n = eval y (ADD n : c)
exec (EVALmul y : c)   n = eval y (MULT n : c)
exec (ADD n : c)    m = exec c (n+m)
exec (MULT n : c)   m = exec c (n*m)
value :: Expr -> Int
value e = eval e []
--8.8
data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop
            | Or Prop Prop
            | Equiv Prop Prop
type Bit = Int
type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']
evalT :: Subst -> Prop -> Bool
evalT _ (Const b)    = b
evalT s (Var x)      = find x s
evalT s (Not p)      = not (evalT s p)
evalT s (And p q)    = evalT s p && evalT s q
evalT s (Imply p q)  = evalT s p <= evalT s q
evalT s (Or p q)     = evalT s p || evalT s q
evalT s (Equiv p q)  = evalT s p == evalT s q
vars :: Prop -> [Char]
vars (Const _)  = []
vars (Var x)    = [x]
vars (Not p)    = vars p
vars (And p q)  = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q
rmdups :: Eq a => [a] -> [a]
rmdups []   = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)
bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin) range
          where
            range = [0..(2^n)-1]
            make n bs = take n (bs ++ repeat 0)
            conv 0 = False
            conv 1 = True
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)
isTaut :: Prop -> Bool
isTaut p = and [evalT s p | s <- substs p]
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Or (Var 'A') (Not (Var 'A'))
p5 :: Prop
p5 = Equiv (Var 'A') (Var 'B')
--8.1
data Nat = Zero | Succ Nat
nat2int :: Nat -> Int
nat2int Zero  = 0
nat2int (Succ n) = 1 + nat2int n
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))
add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)
mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m n = add (mult m (int2nat(nat2int(n)-1))) m
--8.2
data Tree a = Leaf a | Node (Tree a) a (Tree a)
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)              = x == y
occurs x (Node l y r) | (compare x y  == EQ) = True
                      | (compare x y == LT)  = occurs x l
                      | otherwise            = occurs x r
--better definition of occurs:
--occurs x (Leaf y)         = x == y
--occurs x (Node l y r)     = case compare x y of
--                              LT -> occurs x l
--                              EQ -> True
--                              GT -> occurs x r
--This version is more efficient because compare x y is executed only once (requiring 1 comparison), while the original definition 
--of occurs has to perform 2 comparisons in the worst case
--8.3
data Tree2 b = Leaf2 b | Node2 (Tree2 b) (Tree2 b)
numLeaves :: Tree2 b -> Int
numLeaves (Leaf2 lf)         = 1
numLeaves (Node2 l r)        = 0 + (numLeaves l) + (numLeaves r)
balanced :: Tree2 a -> Bool
balanced (Leaf2 lf)          = True
balanced (Node2 l r)         = ((numLeaves l - numLeaves r) <= 1) && (balanced l) && (balanced r)
--8.4
splitInHalf :: [a] -> ([a],[a])
splitInHalf [] = ([],[])
splitInHalf xs = splitAt (length xs `div` 2) xs
balance :: [a] -> Tree2 a
balance [x] = Leaf2 x
balance xs = Node2 (balance (fst (splitInHalf xs))) (balance (snd (splitInHalf xs)))
--8.5
data Expr' = Val' Int | Add' Expr' Expr'
folde :: (Int -> a) -> (a -> a -> a) -> Expr' -> a
folde f g (Val' n) = f n
folde f g (Add' x y) = g (folde f g (x)) (folde f g (y))

main = do
    print $ value (Add (Add (Val 2) (Val 3)) (Val 4))
    print $ value (Mult (Val 2) (Add (Val 4) (Val 5)))
    print $ isTaut p3
    print $ isTaut p4
    print $ isTaut p5
    print $ nat2int(add (int2nat(4)) (int2nat(6)))
    print $ nat2int(mult (Succ(Succ Zero)) (Succ(Succ Zero)))
    print $ occurs 4 (Node (Leaf 2) 3 (Node (Leaf 3) 4 (Leaf 7)))
    print $ numLeaves (Node2 (Node2 (Leaf2 'a') (Leaf2 'b')) (Leaf2 'f'))
    print $ balanced (Node2 (Node2 (Leaf2 'a') (Node2 (Leaf2 't') (Leaf2 'p'))) (Leaf2 'f'))
    print $ balanced (balance [1,2,3,4,5,6,7,8,9,10,11])
    print $ numLeaves (balance [1,2,3,4,5,6,7,8,9])