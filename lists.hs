import Prelude hiding
  (elem, (++), (!!), null, length,
   init, last, take, drop, sum, const, ($),
   map, filter, scanr, scanl, takeWhile)

x :: (Int, String)
x = (3, "abc")

type Vector = (Double, Double)

-- v1 .+ v2 = (fst v1 + fst v2, snd v1 + snd v2)
(x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)

pow (x,0) = 1
pow (x,n) = x * pow(x,n-1)

{-
l1 ++ l2 = if null l1 then l2
           else head l1 : tail l1 ++ l2

l !! n
  | n == 0    = head l
  | otherwise = tail l !! (n-1)
-}
{-
elem x l
 | null l      = False
 | head l == x = True
 | otherwise   = elem x (tail l)
-}

{-
elem x l = not (null l) && (head l == x || elem x (tail l))
-}

null [] = True
null _  = False

length []     = 0
length (_:xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[]     ++ l2 = l2
(x:xs) ++ l2 = x:xs ++ l2

(!!) :: [a] -> Int -> a
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
{-
elem _ []     = False
elem x (y:ys) = x == y || elem x ys
-}
elem x = any (x==)

type Int3 = [(Int,Int,Int)]
pythagoreanTriple :: Int -> Int3
pythagoreanTriple n =
  [ (x, y, z) | x <- [1..n], y <- [1..x-1],
    z <- [max x y..n], x^2 + y^2 == z^2 ]

init :: [a] -> [a]
init [x]    = []
init (x:xs) = x:init xs

last :: [a] -> a
last [x]    = x
last (_:xs) = last xs

take :: Int -> [a] -> [a]
take 0 _= []
take n (x:xs)
  | n < 0   = error "Отрицателно число!"
  | otherwise = x:take (n-1) xs

drop :: Int -> [a] -> [a]
drop 0 l = l
drop n (_:xs)
  | n < 0   = error "Отрицателно число!"
  | otherwise = drop (n-1) xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

const x = \y -> x

-- mult3 = \x -> x * 3
mult3 = (*3)

drop3 = drop 3

pow3 = \x -> pow (3,x)

pow' _ 0 = 1
pow' x n = x * pow' x (n-1)

f $ x = f x

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x -> (f x:)) []

filter _ [] = []
filter p (x:xs)
 | p x       = x:filter p xs
 | otherwise = filter p xs

{-
filter p = foldr
           (\x -> if p x then (x:) else id)
           []
-}

scanr :: (a -> b -> b) -> b -> [a] -> [b]

{-
scanr _  nv []     = [nv]
scanr op nv (x:xs) = x `op` r : rest
  where rest@(r:_) = scanr op nv xs
-}

scanr op nv = foldr
              (\x rest@(r:_) -> x `op` r : rest)
                [nv]

takeWhile p = foldr
              (\x r -> if p x then x:r else [])
              []

sorted l = all (\(x,y) -> x <= y)
           (zip l (tail l))
