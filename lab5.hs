primesMy :: [Integer]
primesMy = 2 : [x | x <- [3..], isPrimes x primesMy]
  where
    isPrimes x (y:ys)
      | y * y > x = True
      | x `mod` y == 0 = False
      | otherwise = isPrimes x ys


takePrimes :: Integer -> [Integer]
takePrimes n = filter (<n) primesMy

factorizeMy :: Integer -> [Integer]
factorizeMy a = go a takePrimes a
  where
    go x (y:ys)
      |