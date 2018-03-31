> module Bird.Pearls.Countdown(countdown, subseqs, mkExpr) where

> countdown :: Int -> [Int] -> (Expr, Value)
> countdown = countdown1

> data Expr = Num Int 
>   | App Op Expr Expr
>   deriving(Show, Eq)

> data Op = Add | Sub | Mul | Div
>   deriving(Show, Eq)

> type Value = Int

> countdown1 :: Int -> [Int] -> (Expr, Value)
> countdown1 n = nearest n . concatMap mkExpr . subseqs

> subseqs :: [a] -> [[a]]
> subseqs [x] = [[x]]
> subseqs (x:xs) = xss ++ [x]:map (x:) xss
>   where xss = subseqs xs

> mkExpr :: [Int] -> [(Expr, Value)]
> mkExpr [x]    = [(Num x, x)]
> mkExpr xs     = [exprs | (ys, zs) <- unmerge2 xs,
>                        e1 <- mkExpr ys,
>                        e2 <- mkExpr zs,
>                        exprs <- combine e1 e2]

> nearest :: Int -> [(Expr, Value)] -> (Expr, Value)
> nearest n ((e,v):evs) = if d == 0
>                         then (e,v)
>                         else search n d (e,v) evs
>   where
>       d = abs (n-v)
>       search n d (e, v) []       = (e,v)
>       search n d ev ((e, v):evs) | d' == 0   = (e,v)
>                                  | d' < d    = search n d' (e,v) evs
>                                  | otherwise = search n d ev evs
>           where d' = abs (n-v)

> unmerge1 :: [a] -> [([a], [a])]
> unmerge1 [x, y] = [([x],[y]), ([y],[x])]
> unmerge1 (x:xs) = [([x], xs), (xs, [x])] ++ concatMap (add x) (unmerge1 xs)
>   where add x (y,z) = [(x:y, z), (y, x:z)]

> combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
> combine (le, lv) (re, rv) = [(App op le re , apply op lv rv) | op <- [Add, Sub, Mul, Div], 
>                                                                legal2 op lv rv]

> value :: Expr -> Value
> value (Num i) = i
> value (App op l r) = apply op (value l) (value r)

> apply Add = (+)
> apply Sub = (-)
> apply Mul = (*)
> apply Div = div

> legal1 :: Op -> Value -> Value -> Bool
> legal1 Add _ _ = True
> legal1 Sub l r = l > r
> legal1 Mul _ _ = True
> legal1 Div l r = l `mod` r == 0


eliminate redundant checks by making the "legal" constraint more stronger
As we are assuming the input is ordered, just look for operands to + to be ordered 
this will eliminate checking both x+y and y+x. Same with multiplication along with 
the added constraint to remove the identity conditions.

With the function `combine' defined in terms of `legal1` it takes about ~16 sec:
*Main Bird Bird.Pearls.Countdown Lib> (countdown 831 [1,3,7,10,25,50])
(App Add (Num 7) (App Mul (App Add (Num 1) (Num 10)) (App Add (Num 25) (Num 50))),832)
(15.82 secs, 7,790,112,024 bytes)

redefining in terms of `legal2`, it takes about ~2 sec:
*Main Bird Bird.Pearls.Countdown Lib> (countdown 831 [1,3,7,10,25,50])
(App Add (Num 7) (App Mul (App Add (Num 1) (Num 10)) (App Add (Num 25) (Num 50))),832)
(1.83 secs, 905,793,336 bytes)

> legal2 :: Op -> Value -> Value -> Bool
> legal2 Add l r = (l <= r)
> legal2 Sub l r = l > r
> legal2 Mul l r = (1 < l) && (l <= r)
> legal2 Div l r = (1 < r) && (l `mod` r == 0)


following with optimizations: the unmerge function
we are generating (ys, zs) and also (zs, ys). no need to do that.

> unmerge2 :: [a] -> [([a], [a])]
> unmerge2 [x, y] = [([x],[y])]
> unmerge2 (x:xs) = [([x], xs)] ++ concatMap (add x) (unmerge2 xs)
>   where add x (y,z) = [(x:y, z), (y, x:z)]


This seems to bring it down to less than a sec!!
*Main Bird Bird.Pearls.Countdown Lib> (countdown 831 [1,3,7,10,25,50])
(App Add (Num 7) (App Mul (App Add (Num 1) (Num 10)) (App Add (Num 25) (Num 50))),832)
(0.13 secs, 59,716,480 bytes)


With memoization:

countdownMemo :: forall a. Memo a => Int -> [Int] ->  (Expr, Value)
countdownMemo n = nearest n . (toList :: a -> [(Expr, Value)]) . memoize . subseqs

> class Memo a where
>   empty :: a
>   fetch :: a -> [Int] -> [(Expr, Value)]
>   store :: [Int] -> [(Expr, Value)] -> a -> a
>   toList :: a -> [(Expr, Value)]
>   memoize :: [[Int]] -> a
>   memoize = foldl insert empty
>   insert :: a -> [Int] -> a
>   insert memo xs = store xs (mkExprMemo memo xs) memo

> mkExprMemo :: Memo a => a -> [Int] -> [(Expr, Value)]
> mkExprMemo _ [x] = [(Num x, x)]
> mkExprMemo memo xs = [ev | (ys, zs) <- unmerge2 xs,
>                             l <- fetch memo ys,
>                             r <- fetch memo zs,
>                             ev <- combine l r]