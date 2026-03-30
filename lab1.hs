module Lab1 where
        
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
