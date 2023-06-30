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

main = do
    print $ value (Add (Add (Val 2) (Val 3)) (Val 4))
    print $ value (Mult (Val 2) (Add (Val 4) (Val 5)))
    print $ isTaut p3
    print $ isTaut p4
    print $ isTaut p5