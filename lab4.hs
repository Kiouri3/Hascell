import Data.Char
import Data.List

-- Строит бесконечный список, последовательно применяя функцию к значению.
iterateMy :: (a -> a) -> a -> [a]
iterateMy f x = x : iterateMy f (f x)

-- Создаёт бесконечный список из одного и того же элемента.
repeatMy :: a -> [a]
repeatMy a =  a : repeatMy a

-- Повторяет заданный список по кругу бесконечно.
cycleMy :: [a] -> [a]
cycleMy [] = error "empty list"
cycleMy xs = xs ++ cycleMy xs

-- Псевдослучайные числа (линейный конгруэнтный метод)
pseudoRandomMy :: Integer -> [Integer]
pseudoRandomMy = iterateMy next
  where
    a = 1103515245
    c = 12345
    m = 2^31
    next x = (a*x + c) `mod` m

-- Беск. список натуральных чисел.
-- show - преобразует значение в строку
-- concatMap - преобразует каждое значение в строку и соединяет в один список
naturalDigits :: [Char]
naturalDigits = concatMap show [1..]

first100Digits :: String
first100Digits = take 100 naturalDigits

-- выводит 1000 символ
thousandthDigit :: Char
thousandthDigit = naturalDigits !! 999

-- putChar :: Char -> IO ()
-- Выводит один символ в стандартный вывод.

-- putStr :: String -> IO ()
-- Выводит строку в стандартный вывод без добавления символа новой строки.

-- putStrLn :: String -> IO ()
-- Выводит строку в стандартный вывод и добавляет символ новой строки в конце.

-- print :: Show a => a -> IO ()
-- Выводит значение любого типа, принадлежащего классу Show,
-- предварительно преобразуя его в строку.

-- getChar :: IO Char
-- Считывает один символ из стандартного ввода.

-- getLine :: IO String
-- Считывает одну строку из стандартного ввода до символа новой строки.

-- readFile :: FilePath -> IO String
-- Читает содержимое файла по заданному пути и возвращает его как строку.

-- writeFile :: FilePath -> String -> IO ()
-- Записывает строку в файл по заданному пути, полностью заменяя его содержимое.

-- interact :: (String -> String) -> IO ()
-- Читает весь ввод как строку, применяет к нему функцию
-- и выводит результат в стандартный вывод.

-- Скобки

matchingBracket :: Char -> Char -> Bool
matchingBracket '(' ')' = True
matchingBracket '[' ']' = True
matchingBracket '{' '}' = True
matchingBracket _   _   = False

isOpenBracket :: Char -> Bool
isOpenBracket c = c `elem` "([{"

isCloseBracket :: Char -> Bool
isCloseBracket c = c `elem` ")]}"

checkBrackets :: String -> Bool
checkBrackets =
    null . foldl step [] . filter (\c -> isOpenBracket c || isCloseBracket c)
  where
    step [] c
        | isOpenBracket c = [c]
        | otherwise = ['#']
    step st c
        | st == ['#'] = ['#']
        | isOpenBracket c = c : st
        | isCloseBracket c =
            case st of
                (x:xs)
                    | matchingBracket x c -> xs
                    | otherwise -> ['#']
                [] -> ['#']
        | otherwise = st

checkBracketsFile :: FilePath -> IO ()
checkBracketsFile input = do
    content <- readFile input
    putStrLn $
        if checkBrackets content
           then "correct"
           else "incorrect"


-- Слова: (слово:число), по убыванию числа

wordCountPairs :: String -> [(String, Int)]
wordCountPairs =
    sortBy (\(_, a) (_, b) -> compare b a) .
    map (\xs -> (head xs, length xs)) .
    group .
    sort .
    wordsNormalized

wordCountFile :: FilePath -> FilePath -> IO ()
wordCountFile input output = do
    content <- readFile input
    writeFile output (renderPairs (wordCountPairs content))

normalizeWord :: String -> String
normalizeWord =
    map toLower . filter isAlphaNum

wordsNormalized :: String -> [String]
wordsNormalized =
    filter (not . null) . map normalizeWord . words

renderPairs :: [(String, Int)] -> String
renderPairs =
    unlines . map (\(w, n) -> w ++ ":" ++ show n)

-- вставляет заданный элемент между всеми элементами заданного списка
intersperseMy :: a -> [a] -> [a]
intersperseMy _ [] = []
intersperseMy _ [x] = [x] 
intersperseMy c (x:xs) = x : c : intersperseMy c xs

-- разбивает заданный список элементов на подсписки в тех местах, где встречается заданный элемент.
explodeMy :: Eq a => a -> [a] -> [[a]]
explodeMy a = foldr step [[]]
  where 
    step x (y:ys)
      | x == a = [] : y : ys
      | otherwise = (x : y) : ys

explodeByMy :: (a -> Bool) -> [a] -> [([a], [a])]
explodeByMy _ [] = []
explodeByMy p xs =
    case span (not . p) xs of
        (_, []) -> []
        (as, rest) ->
            case span p rest of
                (bs, cs) ->
                    (as, bs) : explodeByMy p cs

-- группирует подряд идущие одинаковые элементы в отдельный подсписок
groupMy :: Eq a => [a] -> [[a]]
groupMy = foldr step [[]]
    where
        step x [] = [[x]]
        step x (y:ys)
            | x == head y = (x : y) : ys
            | otherwise = [x] : y : ys

-- делает то же самое, что и group, но по задаваемой операции сравнения.
groupByMy :: (a -> a -> Bool) -> [a] -> [[a]]
groupByMy f = foldr step [[]]
    where
        step x [] = [[x]]
        step x (y:ys)
            | f x (head y) = (x : y) : ys
            | otherwise = [x] : y : ys

-- находит все префиксы заданного списка
-- scanl - возвращает список промежуточных значений
initsMy :: [a] -> [[a]]
initsMy = scanl (\acc x -> acc ++ [x]) []

tailMy :: [a] -> [[a]]
tailMy = scanr (:) []

infixesMy :: [a] -> [[a]]
infixesMy xs =
    [] : go 1
  where
    ts = init (tailMy xs)

    go n
        | n > length xs = []
        | otherwise =
            prefixesOfLen n ts ++ go (n + 1)

    prefixesOfLen _ [] = []
    prefixesOfLen n (y:ys)
        | length y < n = prefixesOfLen n ys
        | otherwise = take n y : prefixesOfLen n ys

-- проверяет, является ли одна строка префиксом другой
isPrefixOfMy :: Eq a => [a] -> [a] -> Bool
isPrefixOfMy xs ys = xs `elem` initsMy ys

-- проверяет, является ли одна строка суффиксом другой
isSuffixOfMy :: Eq a => [a] -> [a] -> Bool
isSuffixOfMy xs ys = xs `elem` tailMy ys

-- проверяет, является ли одна строка инфиниксом другой
isInfixOfMy :: Eq a => [a] -> [a] -> Bool
isInfixOfMy xs ys = xs `elem` infixesMy ys

-- вычисляет кумулятивные суммы всех постфиксов списка
-- Убираем посл. элемент с помощью init, т.к. в конце 0
cumSumPostfixMy :: Num a => [a] -> [a]
cumSumPostfixMy = init . scanr (+) 0

-- вычисляет кумулятивные суммы всех префиксов списка
cumSumPrefixMy :: Num a => [a] -> [a]
cumSumPrefixMy = tail . scanl (+) 0

-- вычисляет разницы между парами подряд идущих чисел
diffMy :: [Int] -> [Int]
diffMy xs = zipWith (-) (tail xs) xs

-- находит для каждого числа списка, сколько чисел имеется строго меньше него
countLessMy :: [Int] -> [Int]
countLessMy xs = map go xs
    where
        go x = length(filter (<x) xs)