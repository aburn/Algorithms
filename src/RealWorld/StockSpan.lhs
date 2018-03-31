> {-# LANGUAGE BangPatterns #-}

> module RealWorld.StockSpan (
>   stockSpan,
>   stockSpanQuad,
>   stockSpanLinear,
> ) where

> import Data.List(inits)

> type Quote = Int
> type Quotes = [Quote]
> type Span = [Int]

> stockSpan :: Quotes -> Span
> stockSpan = stockSpanLinear

O(n^2) algorithm for calculating the stock span. 

> stockSpanQuad :: Quotes -> Span
> stockSpanQuad [] = []
> stockSpanQuad xs = map spanQ (map splitfunc  (tail $ inits xs))
>   where
>       spanQ (qs, q) = 1 + (length $ takeWhile (\a -> a <= q) (reverse qs))
>       splitfunc xs = (init xs, last xs)

> stockSpanLinear :: Quotes -> Span
> stockSpanLinear = reverse.snd.(foldl func ([],[]))

 foldl func [] [100, 80, 60, 70, 60, 75, 85]
 
> type Stack = [(Quote, Int)]

> func ::  (Stack, Span)-> Quote -> (Stack, Span)
> func ([], []) q             = ([(q, 1)], [1])
> func p@((_, !i):pis, span) q = go p q (i+1)
>    where
>        go :: (Stack, Span) -> Quote -> Int -> (Stack, Span)
>        go (stack,span) q index = let ys = dropWhile (\(p, _) -> p <= q) stack
>                           in case ys of
>                                []      -> ((q, index):ys, index+1:span)
>                                (_,i):_ -> ((q, index):ys, index-i:span)


stockSpanWithStack :: Quotes -> Span
stockSpanWithStack quotes = calculateSpan quotesWithIndexes []
  where
    quotesWithIndexes = zip quotes [0..]
    calculateSpan [] _ = []
    calculateSpan ((x, index):xs) stack =
      let
        newStack = dropWhile (\(y, _) -> y <= x) stack
        stockValue [] = index + 1
        stockValue ((_, x):_) = index - x
      in
        (stockValue newStack) : (calculateSpan xs ((x, index):newStack))
