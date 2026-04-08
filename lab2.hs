import Data.Char

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

  