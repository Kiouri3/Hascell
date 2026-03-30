-- module Lab1 where

-- (+) ::   Num a => a -> a -> a
-- (-) ::   Num a => a -> a -> a
-- (*) ::   Num a => a -> a -> a
-- (/) ::   Fractional a => a -> a -> a
-- div ::   Integral a => a -> a -> a
-- mod ::   Integral a => a -> a -> a
-- (^) ::   (Num a, Integral b) => a -> b -> a

sumMy [] = 0
sumMy(x:xs) = x + sumMy xs

productMy [] = 1
productMy (x:xs) = x * productMy xs

maxMy x y
        | x >= y = x
        | otherwise = y

minMy x y
        | x <= y = x
        | otherwise = y

maximumMy [] = error "empty list"
maximumMy [x] = x
maximumMy (x:xs) = maxMy x (maximumMy xs)

minimumMy [] = error "empty list"
minimumMy [x] = x
minimumMy (x:xs) = minMy x (minimumMy xs)

-- Проверка на четность, mod - деление с остатком (%)
evenMy x = mod x 2 == 0

-- Проверка на нечетность
oddMy x = mod x 2 /= 0

-- Наибольший общий делитель
gcdMy a 0 = a
gcdMy a b = gcdMy b (mod a b)

-- Формула НОК: a * b / НОД(a, b)
lcmMy a 0 = 0
lcmMy 0 b = 0
lcmMy a b = div (a * b) (gcdMy a b)