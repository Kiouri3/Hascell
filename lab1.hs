import Data.Char (isLower, isUpper, toLower, toUpper, isLetter)

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
factMy 0 = 1
factMy a
        | a < 0 = error "negative argument"
        | otherwise = a* factMy (a - 1)

fibMy 0 = 0
fibMy 1 = 1
fibMy n = fibMy (n - 1) + fibMy (n - 2)

fibMy' 0 prev prevprev = prev
fibMy' 1 prev prevprev = prevprev
fibMy' n prev prevprev = fibMy' (n - 1) prevprev (prev + prevprev)

fastFibMy n = fibMy' n 0 1

-- (>) :: Ord a => a -> a -> Bool
-- (<) :: Ord a => a -> a -> Bool
-- (==) :: Eq a => a -> a -> Bool
-- (/=) ::  Eq a => a -> a -> Bool
-- (>=) ::  Ord a => a -> a -> Bool
-- (<=) ::  Ord a => a -> a -> Bool
-- (&&) ::  Bool -> Bool -> Bool
-- (||) ::  Bool -> Bool -> Bool
-- not  ::  Bool -> Bool

andMy [] = True
andMy (x:xs) = x && andMy xs

orMy [] = False
orMy (x:xs) = x || orMy xs

-- (:) :: a -> [a] -> [a]             добавляет элемент в начало списка
-- null :: Foldable t => t a -> Bool  проверяет, пустой ли список.

headMy [] = error "empty list"
headMy (x:xs) = x

tailMy [] = error "Empty list"
tailMy (x:xs) = xs

-- Возвращает последний элемент списка
lastMy [] = error "Empty list"
lastMy [a] = a
lastMy (x:xs) = lastMy xs

-- Возвращает все элементы списка, кроме последнего.
initMy [] = error "Empty list"
initMy [_] = [] 
initMy (x:xs) = x : initMy xs

lengthMy [] = 0
lengthMy (x:xs) = lengthMy xs + 1

indexMy [] n = error "invalid index"
indexMy (x:xs) 0 = x
indexMy (x:xs) n = indexMy xs (n - 1)

appendMy [] ys = ys
appendMy (x:xs) ys = x : appendMy xs ys

-- объединяет список списков
concatMy [] = []
concatMy (x:xs) = appendMy x (concatMy xs)

-- взять первые n
takeMy 0 _ = []
takeMy _ [] = []
takeMy n (x:xs) = x : takeMy (n - 1) xs

-- убрать первые n
dropMy 0 list = list
dropMy _ [] = []
dropMy n (x:xs) = dropMy (n - 1) xs

reverseMy [] = []
reverseMy (x:xs) = reverse xs  ++ [x]

-- есть ли элемент
elemMy _ [] = False
elemMy y (x:xs) 
        | y == x = True
        | otherwise = elemMy y xs
        
-- n копий элемента
replicateMy a 1 = [a]
replicateMy a n = a : replicateMy a (n - 1) 

lookupMy :: Eq a => a -> b -> [(a, b)] -> b
lookupMy _ def [] = def
lookupMy key def ((first, second):xs) 
        | key == first = second
        | otherwise = lookupMy key def xs

substrMy :: [a] -> Int -> Int -> [a]
substrMy xs a b = take (b - a) (drop a xs)

strReplaceMy :: Eq a => [a] -> [a] -> [a] -> [a]

strReplaceMy old new xs
        | length old == 0 = xs
        | otherwise = replace xs
        where
                lenOld = length old
                replace [] = []
                replace ys
                        | take lenOld ys == old = new ++ replace (drop lenOld ys)
                        | otherwise = head ys : replace (tail ys)
                        
elemIndicesMy :: Eq a => a -> [a] -> [Int]
elemIndicesMy elem xs = go xs 0
        where
                go [] _ = []
                go (y:ys) i
                        | elem == y = i : go ys (i + 1)
                        | otherwise = go ys (i + 1)

strPosMy :: Eq a => [a] -> [a] -> [Int]
strPosMy sub xs = go xs 0
        where
                lenSub = length sub
                go [] _ = []
                go ys i
                        | length ys < lenSub = []
                        | take lenSub ys == sub = i : go (tail ys) (i + 1)
                        | otherwise = go (tail ys) (i + 1)

strRotateMy :: [a] -> Int -> [a]
strRotateMy [] _ = []
strRotateMy xs n = rotateTimes xs m
        where
                len = length xs
                m = mod n len

                rotateTimes ys 0 = ys
                rotateTimes ys k = rotateTimes (last ys : init ys) (k - 1)

unevenHandWritingMy :: String -> String
unevenHandWritingMy xs = go xs 1
        where
                go [] _ = []
                go (c:cs) i
                        | mod i 3 == 0 = swapCase c : go cs (i + 1)
                        | otherwise = c : go cs (i + 1)

                swapCase ch
                        | isLower ch = toUpper ch
                        | isUpper ch = toLower ch
                        | otherwise = ch