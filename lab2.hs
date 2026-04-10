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
hexToDecMy str = go str 0
  where
    go :: String -> Integer -> Integer
    go [] acc = acc
    go (c:cs) acc =
        go cs (acc * 16 + toInteger (digitToIntMy c))

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

-- Удаляет дубликаты из списка
nubMy :: Eq a => [a] -> [a]
nubMy [] = []
nubMy (x:xs) = x : nubMy (filter (/= x) xs)

-- Удаляет первое вхождение заданного элемента из списка
deleteMy :: Eq a => a -> [a] -> [a]
deleteMy a (x:xs)
  | a == x = xs
  | otherwise = x : deleteMy a xs

-- объединяет два списка(как множества)
unionMy :: Eq a => [a] -> [a] -> [a]
unionMy xs ys = nubMy (xs ++ ys)

-- Вычитает из первого списка второй(как множества)
diffMy :: Eq a => [a] -> [a] -> [a]
diffMy xs ys = go (nubMy xs) (nubMy ys)
  where
    go :: Eq a => [a] -> [a] -> [a]
    go xs [] = xs
    go xs (y:ys) = go (deleteMy y xs) ys

-- Находит пересечение двух множеств
intersectMy :: Eq a => [a] -> [a] -> [a]
intersectMy xs ys = go (nubMy xs) ys
  where
    go [] _ = []
    go (x:xs') ys'
      | x `elem` ys' = x : go xs' ys'
      | otherwise    = go xs' ys'

-- Строит булеан множества, то есть список всех подмножеств.
powersetMy :: [a] -> [[a]]
powersetMy [] = [[]]
powersetMy (x:xs) =
    appendWithX x ps ++ ps
  where
    ps = powersetMy xs

-- Добавляет элемент x в начало каждого подсписка.
appendWithX :: a -> [[a]] -> [[a]]
appendWithX _ [] = []
appendWithX x (s:ss) =
    (x:s) : appendWithX x ss
-- Для каждого подмножества строит пару: само подмножество, его дополнение до исходного множества
complementsMy :: Eq a => [a] -> [([a],[a])]
complementsMy xs = go (powersetMy setXs)
  where
    setXs = nubMy xs

    go [] = []
    go (s:ss) = (s, diffMy setXs s) : go ss

-- Вставляет элемент в уже отсортированный список
insertMy :: Ord a => a -> [a] -> [a]
insertMy x [] = [x]
insertMy x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insertMy x ys

-- Сортирует список по возрастанию
sortMy :: Ord a => [a] -> [a]
sortMy [] = []
sortMy (x:xs) = insertMy x (sortMy xs)

-- подсчитывает количество вхождений каждого символа в строку и выводит список кортежей, 
-- отсортированный по убыванию второго элемента кортежа
countCharsMy :: String -> [(Char, Int)]
countCharsMy str = sortPairsDesc (makeCounts uniqueChars str)
  where
    uniqueChars = nubMy str

-- Вспом.фукция, строит пару
makeCounts :: String -> String -> [(Char, Int)]
makeCounts [] _ = []
makeCounts (c:cs) str =
    (c, countChar c str) : makeCounts cs str

-- Считает кол-во вхождений символа
countChar :: Char -> String -> Int
countChar _ [] = 0
countChar c (x:xs)
  | c == x    = 1 + countChar c xs
  | otherwise = countChar c xs


-- Сортирует список пар 
sortPairsDesc :: [(Char, Int)] -> [(Char, Int)]
sortPairsDesc [] = []
sortPairsDesc (x:xs) = insertPairDesc x (sortPairsDesc xs)

-- Вставляет одну пару в уже отсортированный список пар
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