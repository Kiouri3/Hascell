import Data.Char
import Debug.Trace

-- isUpper — проверяет, является ли символ заглавной буквой.
-- isLower — проверяет, является ли символ строчной буквой.
-- isAlpha — проверяет, является ли символ буквой.
-- isDigit — проверяет, является ли символ цифрой.
-- toUpper — переводит символ в верхний регистр.
-- toLower — переводит символ в нижний регистр.
-- ord возвращает числовой код символа.
-- chr строит символ по его числовому коду.
-- digitToInt преобразует символ цифры в число.
-- intToDigit преобразует число от 0 до 15 в символ шестнадцатеричной цифры.

digitToIntMy :: Char -> Int
digitToIntMy c
  | c >= '0' && c <= '9' = ord c - ord '0'
  | c >= 'a' && c <= 'f' = 10 + ord c - ord 'a'
  | c >= 'A' && c <= 'F' = 10 + ord c - ord 'A'
  | otherwise = error "not a digit"

intToDigitMy :: Int -> Char
intToDigitMy n
  | n >= 0 && n <= 9  = chr (ord '0' + n)
  | n >= 10 && n <= 15 = chr (ord 'a' + (n - 10))
  | otherwise = error "out of range"

-- Перевод из 16 в 10 с помощью алгоритма Горнера
hexToDecMy :: String -> Integer
hexToDecMy str = helper str 0
  where
    helper :: String -> Integer -> Integer
    helper [] acc = acc
    helper (c:cs) acc =
        helper cs (acc * 16 + toInteger (digitToIntMy c))

-- перевод из 10-ричной в 16-ричную
decToHexMy :: Integer -> String
decToHexMy n
  | n < 0     = error "negative number"
  | n == 0    = "0"
  | otherwise = reverse (toHex n)

toHex :: Integer -> String
toHex 0 = ""
toHex x =
    intToDigitMy (fromInteger (x `mod` 16))
    : toHex (x `div` 16)

-- Перевод с арабских на римские цифры(до 3999)
arabToRomanMy :: Integer -> String
arabToRomanMy n
  | n <= 0 || n > 3999 = error "number out of range"
  | otherwise = go n romanTable
  where
    romanTable =
      [ (1000, "M"), (900, "CM"), (500, "D"), (400, "CD")
      , (100, "C"), (90, "XC"), (50, "L"), (40, "XL")
      , (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")
      ]

    go 0 _ = ""
    go x ((v,s):xs)
      | x >= v    = s ++ go (x - v) ((v,s):xs)
      | otherwise = go x xs
    go _ [] = ""

-- Перевод с Римских на арабские
romanToArabMy :: String -> Integer
romanToArabMy [] = 0
romanToArabMy [x] = value x
romanToArabMy (x:y:xs)
  | value x < value y = value y - value x + romanToArabMy xs
  | otherwise = value x + romanToArabMy (y:xs)

value :: Char -> Integer
value 'I' = 1
value 'V' = 5
value 'X' = 10
value 'L' = 50
value 'C' = 100
value 'D' = 500
value 'M' = 1000
value _   = error "invalid Roman digit"

-- fst возвращает первый элемент пары.
-- snd возвращает второй элемент пары.

-- zip объединяет два списка в список кортежей.
-- unzip делает обратное: из списка кортежей строит кортеж списков.

zipMy :: [a] -> [b] -> [(a,b)]
zipMy [] _ = []
zipMy _ [] = []
zipMy (x:xs) (y:ys) = (x, y) : zipMy xs ys

unzipMy :: [(a,b)] -> ([a],[b])
unzipMy [] = ([], [])
unzipMy ((x, y):xs) = (x:as, y:bs)
  where
    (as, bs) = unzipMy xs

nubMy :: Eq a => [a] -> [a]
nubMy [] = []
nubMy (x:xs) = x : nubMy (filter (/= x) xs)

deleteMy :: Eq a => a -> [a] -> [a]
deleteMy a (x:xs)
  | a == x = xs
  | otherwise = x : deleteMy a xs

unionMy :: Eq a => [a] -> [a] -> [a]
unionMy xs ys = nubMy (xs ++ ys)

diffMy :: Eq a => [a] -> [a] -> [a]
diffMy xs ys = go (nubMy xs) (nubMy ys)
  where
    go :: Eq a => [a] -> [a] -> [a]
    go xs [] = xs
    go xs (y:ys) = go (deleteMy y xs) ys

intersectMy :: Eq a => [a] -> [a] -> [a]
intersectMy xs ys = helper (nubMy xs) ys
  where
    helper [] _ = []
    helper (x:xs') ys'
      | x `elem` ys' = x : helper xs' ys'
      | otherwise    = helper xs' ys'

powersetMy :: [a] -> [[a]]
powersetMy [] = [[]]
powersetMy (x:xs) =
    appendWithX x ps ++ ps
  where
    ps = powersetMy xs

appendWithX :: a -> [[a]] -> [[a]]
appendWithX _ [] = []
appendWithX x (s:ss) =
    (x:s) : appendWithX x ss
  
complementsMy :: Eq a => [a] -> [([a],[a])]
complementsMy xs = helper (powersetMy setXs)
  where
    setXs = nubMy xs

    helper [] = []
    helper (s:ss) = (s, diffMy setXs s) : helper ss

insertMy :: Ord a => a -> [a] -> [a]
insertMy x [] = [x]
insertMy x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insertMy x ys

sortMy :: Ord a => [a] -> [a]
sortMy [] = []
sortMy (x:xs) = insertMy x (sortMy xs)

countCharsMy :: String -> [(Char, Int)]
countCharsMy str = sortPairsDesc counts
  where
    chars = nubMy str
    counts = [(c, count c str) | c <- chars]

    count :: Char -> String -> Int
    count ch = length . filter (== ch)

    sortPairsDesc :: [(Char, Int)] -> [(Char, Int)]
    sortPairsDesc [] = []
    sortPairsDesc (x:xs) = insertPairDesc x (sortPairsDesc xs)

    insertPairDesc :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
    insertPairDesc p [] = [p]
    insertPairDesc p@(c1,n1) (q@(c2,n2):qs)
      | n1 >= n2  = p : q : qs
      | otherwise = q : insertPairDesc p qs

-- show преобразует значение в строку.
-- read читает значение из строки.
-- error аварийно завершает программу с сообщением.
-- undefined — специальное неопределённое значение любого типа.


-- trace :: String -> a -> a
-- trace печатает строку для отладки и возвращает второй аргумент без изменения.

factorialMy :: Integer -> Integer
factorialMy n
  | n < 0     = error "negative"
  | n == 0    = trace "factorial 0 = 1" 1
  | otherwise = trace ("factorial " ++ show n) (n * factorialMy (n - 1))