import Prelude hiding (map, filter, all, any, takeWhile, dropWhile, curry, id, iterate)
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


main = do
    print $ value (Add (Add (Val 2) (Val 3)) (Val 4))
    print $ value (Mult (Val 2) (Add (Val 4) (Val 5)))