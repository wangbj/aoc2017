import Data.Char

count xs = sum . zipWith acc xs
  where acc x y | x == y = x
                | x /= y = 0

captcha k xs = count xs (drop k xs ++ take k xs)

solution1 xs = captcha 1 xs
solution2 xs = captcha (length xs `div` 2) xs

readDigits :: String -> [Int]
readDigits = foldr (\x r -> ord x - ord '0' : r) []
