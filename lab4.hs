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

