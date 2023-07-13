import Prelude hiding ()
import Data.Typeable
data Op = Add | Sub | Mul | Div
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
data Expr = Val Int | App Op Expr Expr
instance Show Expr where
    show (Val n)     = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                            brak (Val n) = show n
                            brak e       = "(" ++ show e ++ ")"
values :: Expr -> [Int]
values (Val n)  = [n]
values (App _ l r) = values l ++ values r
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))
--choices :: [a] -> [[a]]
--choices = concat . map perms . subs
--9.1
choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]
--9.2
rmFirstOcc :: Eq a => a -> [a] -> [a]
rmFirstOcc v []     = []
rmFirstOcc v (x:xs) = if (x == v) then xs else [x] ++ (rmFirstOcc v xs)
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice _ [] = False
isChoice [] (y:ys) = True
isChoice (x:xs) (y:ys) = if rmFirstOcc x (y:ys) == (y:ys) then False else isChoice xs (y:ys)
  
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = 
    elem (values e) (choices ns) && eval e == [n]
split :: [a] -> [([a],[a])]
split []        = []
split [_]       = []
split (x:xs)    = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]
exprs :: [Int] -> [Expr]
exprs []    = []
exprs [n]   = [Val n]
exprs ns    = [e | (ls,rs) <- split ns,
                   l       <- exprs ls,
                   r       <- exprs rs,
                   e       <- combine l r]
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]
ops :: [Op]
ops = [Add,Sub,Mul,Div]
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]
{- 9.3
Generalising the function split to also return pairs containing the empty list would cause non-termination of exprs function because
its recursive calls would no longer be guaranteed to reduce the length of the list.
-}
--9.4
allExprs :: [Int] -> [Expr]
allExprs xs = [exp | choice <- choices xs, exp <- exprs choice]
successfuls :: [Expr] -> [Expr]
successfuls exps = [sucExp | sucExp <- exps, eval sucExp >= [0], round (fromIntegral (head (eval sucExp))) >= 1]   
{- 9.5
valid' :: Op' -> Integer -> Integer -> Bool
valid' Add' _ _ = True
valid' Sub' x y = True
valid' Mul' _ _ = True
valid' Div' x y = (y /= 0) && (x `mod` y == 0)
-} 

--9.6
data Op' = Add' | Sub' | Mul' | Div' | Exp'
instance Show Op' where
    show Add' = "+"
    show Sub' = "-"
    show Mul' = "*"
    show Div' = "/"
    show Exp' = "^"
valid' :: Op' -> Integer -> Integer -> Bool
valid' Add' _ _ = True
valid' Sub' x y = x >= y
valid' Mul' _ _ = True
valid' Div' x y = (y /= 0) && (x `mod` y == 0)
valid' Exp' x y = y >= 0
apply' :: Op' -> Integer -> Integer -> Integer
apply' Add' x y = x + y
apply' Sub' x y = x - y
apply' Mul' x y = x * y
apply' Div' x y = x `div` y
apply' Exp' x y = x ^ y 
data Expr' = Val' Integer | App' Op' Expr' Expr'
instance Show Expr' where
    show (Val' n)     = show n
    show (App' o l r) = brak l ++ show o ++ brak r
                       where
                            brak (Val' n) = show n
                            brak e       = "(" ++ show e ++ ")"
values' :: Expr' -> [Integer]
values' (Val' n)  = [n]
values' (App' _ l r) = values' l ++ values' r
eval' :: Expr' -> [Integer]
eval' (Val' n) = [n | n > 0]
eval' (App' o l r) = [apply' o x y | x <- eval' l,
                                  y <- eval' r,
                                  valid' o x y]
subs' :: [a] -> [[a]]
subs' []     = [[]]
subs' (x:xs) = yss ++ map (x:) yss
              where yss = subs' xs
interleave' :: a -> [a] -> [[a]]
interleave' x []     = [[x]]
interleave' x (y:ys) = (x:y:ys) : map (y:) (interleave' x ys)
perms' :: [a] -> [[a]]
perms' []     = [[]]
perms' (x:xs) = concat (map (interleave' x) (perms' xs))
choices' :: [a] -> [[a]]
choices' = concat . map perms' . subs'
solution' :: Expr' -> [Integer] -> Integer -> Bool
solution' e ns n = 
    elem (values' e) (choices' ns) && eval' e == [n]
split' :: [a] -> [([a],[a])]
split' []        = []
split' [_]       = []
split' (x:xs)    = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split' xs]
exprs' :: [Integer] -> [Expr']
exprs' []    = []
exprs' [n]   = [Val' n]
exprs' ns    = [e | (ls,rs) <- split' ns,
                   l       <- exprs' ls,
                   r       <- exprs' rs,
                   e       <- combine' l r]
combine' :: Expr' -> Expr' -> [Expr']
combine' l r = [App' o l r | o <- ops']
ops' :: [Op']
ops' = [Add',Sub',Mul',Div',Exp']
listToInt :: [Integer] -> Integer
listToInt [] = 0
listToInt [x] = x
solutions' :: [Integer] -> Integer -> [Expr']
-- (eval' e == [n] || ((subtract (head (eval' e)) n) <= 1)) makes sure solutions outputs both exact solutions and those that are
-- by 1 smaller or larger than the goal value
solutions' ns n = [e | ns' <- choices' ns, e <- exprs' ns', (eval' e == [n]) || (abs (subtract (listToInt (eval' e)) n) <= 1)]
allExprs' :: [Integer] -> [Expr']
allExprs' xs = [exp | choice <- choices' xs, exp <- exprs' choice]
successfuls' :: [Expr'] -> [Expr']
successfuls' exps = [sucExp | sucExp <- exps, eval' sucExp >= [0], round (fromIntegral (head (eval' sucExp))) >= 1]

main = do
    --print $ show (App Add (Val 1) (App Mul (Val 2) (Val 3)))
    --print $ values (App Add (Val 1) (App Mul (Val 2) (Val 3)))
    --print $ eval (App Add (Val 1) (App Mul (Val 2) (Val 3)))
    --print $ subs [1,2]
    --print $ interleave 1 [2,3,4]
    --print $ perms [1,2,3]
    print $ choices [1,2,3]
    print $ rmFirstOcc 2 [1,2,2,2,3,4,5,2]
    print $ isChoice [3,1,2] [1,2,3]
    print $ solution (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))) [1,3,7,10,25,50] 765
    print $ split [1,2,3,4]
    --print $ solutions [1,3,7,10,25,50] 765
--9.4
    --print $ length (allExprs [1,3,7,10,25,50])
    --print $ length (successfuls (allExprs [1,3,7,10,25,50]))
--9.5
    --print $ length (successfuls' (allExprs' [1,3,7,10,25,50]))
--9.6
    print $ solutions' [2,3,4] 24
    print $ solutions' [2,3,4] 10