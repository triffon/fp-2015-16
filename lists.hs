import Prelude hiding (elem)

x :: (Int, String)
x = (3, "abc")

type Vector = (Double, Double)

-- v1 .+ v2 = (fst v1 + fst v2, snd v1 + snd v2)
(x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)

pow (x,0) = 1
pow (x,n) = x * pow(x,n-1)

l1 +++ l2 = if null l1 then l2 else head l1 : tail l1 +++ l2

l !!! n
 | n == 0    = head l
 | otherwise = tail l !!! (n-1)

{-
elem x l
 | null l      = False
 | head l == x = True
 | otherwise   = elem x (tail l)
-}

elem x l = not (null l) && (head l == x || elem x (tail l))
