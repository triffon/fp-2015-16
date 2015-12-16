x :: Int
x = 2

quickSort []     = []
quickSort (x:xs) = quickSort less ++ [x] ++ quickSort more
  where less = filter (<=x) xs
        more = filter (>x ) xs

{- многоредов
  коментар -}
y :: Double
y = fromIntegral x^2 + 7.5

z :: String
z = "Hello"
-- !!! z = x + y
-- !!! z = "Goodbye"

t :: Int
t = 2^63

square :: Double -> Double
square x = x * x

hypothenuse :: Double -> (Double -> Double)
hypothenuse a b = sqrt (square a + square b)

hyp3 = hypothenuse 3

twice f x = f (f x)

diag f x = f x x

plus1 = (+) 1

-- is5 5 = True
-- is5 _ = False
is5 = (==5)

fact n
 | n == 0   = 1
 | n > 0    = n * fact (n - 1)
