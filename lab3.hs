-- применяет функцию к каждому элементу списка.
mapMy :: (a -> b) -> [a] -> [b]
mapMy f (x:xs) = f x : mapMy f xs

-- оставляет только те элементы, для которых функция f возвращает True.
filterMy :: (a -> Bool) -> [a] -> [a]
filterMy f (x:xs)
  | f x = x : filterMy f xs
  | otherwise = filterMy f xs

-- проверяет, есть ли хотя бы один элемент, для которого функция f истина.
anyMy :: (a -> Bool) -> [a] -> Bool
anyMy _ [] = False
anyMy f (x:xs)
  | f x = True
  | otherwise = anyMy f xs

-- проверяет, истинно ли условие для всех элементов списка.
allMy :: (a -> Bool) -> [a] -> Bool
allMy _ [] = True
allMy f (x:xs)
  | f x == False = False
  | otherwise = allMy f xs

{-
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- Поэлементно применяет бинарную функцию к двум спискам.

(.) :: (b -> c) -> (a -> b) -> a -> c
-- Композиция функций: (f . g) x = f (g x).

($) :: (a -> b) -> a -> b
-- Применение функции к аргументу, часто для уменьшения скобок.

flip :: (a -> b -> c) -> b -> a -> c
-- Меняет порядок двух аргументов бинарной функции.

id :: a -> a
-- Тождественная функция: возвращает аргумент без изменений.

const :: a -> b -> a
-- Всегда возвращает первый аргумент, игнорируя второй.

curry :: ((a, b) -> c) -> a -> b -> c
-- Превращает функцию от пары в функцию от двух аргументов.

uncurry :: (a -> b -> c) -> (a, b) -> c
-- Превращает функцию от двух аргументов в функцию от пары.

splitAt :: Int -> [a] -> ([a], [a])
-- Делит список на две части по указанной позиции.

takeWhile :: (a -> Bool) -> [a] -> [a]
-- Берёт начальный префикс списка, пока предикат истинен.

dropWhile :: (a -> Bool) -> [a] -> [a]
-- Отбрасывает начальный префикс списка, пока предикат истинен.

span :: (a -> Bool) -> [a] -> ([a], [a])
-- Разбивает список на префикс, где предикат истинен, и остаток.

break :: (a -> Bool) -> [a] -> ([a], [a])
-- Разбивает список в первой точке, где предикат становится истинным.

until :: (a -> Bool) -> (a -> a) -> a -> a
-- Повторяет применение функции, пока условие не станет истинным.

concatMap :: (a -> [b]) -> [a] -> [b]
-- Применяет функцию, возвращающую список, ко всем элементам и склеивает результат.

foldl :: (b -> a -> b) -> b -> [a] -> b
-- Левая свёртка списка с начальным аккумулятором.

foldl1 :: (a -> a -> a) -> [a] -> a
-- Левая свёртка непустого списка без явного начального значения.

scanl :: (b -> a -> b) -> b -> [a] -> [b]
-- Левая свёртка с сохранением всех промежуточных результатов.

scanl1 :: (a -> a -> a) -> [a] -> [a]
-- Как scanl, но для непустого списка и без начального значения.

foldr :: (a -> b -> b) -> b -> [a] -> b
-- Правая свёртка списка с начальным аккумулятором.

foldr1 :: (a -> a -> a) -> [a] -> a
-- Правая свёртка непустого списка без явного начального значения.

scanr :: (a -> b -> b) -> b -> [a] -> [b]
-- Правая свёртка с сохранением всех промежуточных результатов.

scanr1 :: (a -> a -> a) -> [a] -> [a]
-- Как scanr, но для непустого списка и без начального значения.
-}

-- Удаляет все гласные буквы
removeVowels :: String -> String
removeVowels = filter (\c -> not (elem c vowels))
  where 
    vowels = "aeiouyAEIOUYаеёиоуыэюяАЕЁИОУЫЭЮЯ"

-- Удаялем все согласные буквы
removeConsonants :: String -> String
removeConsonants = filter (\c -> not (elem c consonants))
  where
    consonants =
      "bcdfghjklmnpqrstvwxzBCDFGHJKLMNPQRSTVWXZбвгджзйклмнпрстфхцчшщБВГДЖЗЙКЛМНПРСТФХЦЧШЩ"

-- Удавялет цифры
removeDigits :: String -> String
removeDigits = filter (\c -> not (elem c "0123456789"))

--  возвращает строку, состоящую из первых букв каждой строки
firstLetters :: [String] -> String
firstLetters = map head

-- возвращает список из последних элементов подсписков
lastsMy :: [[a]] -> [a]
lastsMy = map last

-- берет список списков и возвращающую список из N-х элементов подсписков с помощью функций map и (!!)
nthElementsMy :: [[a]] -> Int -> [a]
nthElementsMy xss n = map (!! n) xss

reverseAllMy :: [[a]] -> [[a]]
reverseAllMy xs = map reverse (reverse xs)

mapIfMy :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapIfMy _ _ _ [] = []
mapIfMy cond f1 f2 (x:xs)
  | cond x = f1 x : mapIfMy cond f1 f2 xs
  | otherwise = f2 x : mapIfMy cond f1 f2 xs

-- id возвращает само значение
composeAllMy :: [a -> a] -> (a -> a)
composeAllMy = foldr (\f g x -> f (g x)) id

applyIterateMy :: [a -> a] -> a -> a
applyIterateMy fs x = foldl (\acc f -> f acc) x fs

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition cond (x:xs)
  | cond x = (x:ts, fs)
  | otherwise = (ts, x:fs)
  where (ts, fs) = partition cond xs

countTruePreds :: [a -> Bool] -> a -> Int
countTruePreds ps x = length (filter (\p -> p x) ps)

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p = go 0
  where
    go _ [] = []
    go n (y:ys)
      | p y = n : go (n + 1) ys
      | otherwise = go (n + 1) ys

sortByMy :: (a -> a -> Ordering) -> [a] -> [a]
sortByMy _ [] = []
sortByMy cmp (x:xs) = insertByMy cmp x (sortByMy cmp xs)

-- Gt означает "больше"
insertByMy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertByMy _ x [] = [x]
insertByMy cmp x (y:ys) =
    if cmp x y == GT
    then y : insertByMy cmp x ys
    else x : y : ys

onMy :: (b -> b -> c) -> (a -> b) -> a -> a -> c
onMy fb fa x y = fb (fa x) (fa y)

filterMapAndMy :: [a -> Bool] -> [a] -> [a]
filterMapAndMy ps = filter (\x -> all (\p -> p x) ps)

filterMapOrMy :: [a -> Bool] -> [a] -> [a]
filterMapOrMy ps = filter (\x -> any (\p -> p x) ps)

sumEqMy :: [[Int]] -> [Int]
sumEqMy = foldr addCols []
  where 
    addCols [] ys = ys
    addCols xs [] = xs
    addCols (x:xs) (y:ys) = (x + y) : addCols xs ys

mapAccumLMy :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumLMy _ acc [] = (acc, [])
mapAccumLMy f acc (x:xs) = combine (f acc x)
  where
    combine (acc1, y) = finish (mapAccumLMy f acc1 xs) y
    finish (acc2, ys) y = (acc2, y:ys)

segregateFMy :: [a -> Bool] -> a -> ([a -> Bool], [a -> Bool])
segregateFMy [] _ = ([], [])
segregateFMy (f:fs) x = add f (segregateFMy fs x)
  where
    add f (ts, fs') =
        if f x
        then (f:ts, fs')
        else (ts, f:fs')