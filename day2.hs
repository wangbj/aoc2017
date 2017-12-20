import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import           Data.Maybe
import           Data.Semigroup
import           Data.List
import           Data.Char

data MinMax a = MinMax a a deriving Show

instance (Ord a, Bounded a) => Monoid (MinMax a) where
  mempty = MinMax maxBound minBound
  MinMax min1 max1 `mappend` MinMax min2 max2 = MinMax (min min1 min2) (max max1 max2)

readInts = unfoldr (fmap (fmap (C.dropWhile isSpace)) . C.readInt)

getDiff (MinMax a b) = Sum (b - a)

lift2 x = MinMax x x

cr :: [Int] -> Sum Int
cr = getDiff . foldMap lift2

checkSum :: [[Int]] -> Int
checkSum = getSum . foldMap cr

getDiv2 ints = [ x `div` y | (i, x) <- xs, (j, y) <- xs, i /= j, x `mod` y == 0] where xs = zip [1..] ints

checkSum2 :: [[Int]] -> Int
checkSum2 = sum . concatMap getDiv2

--main1 = C.getContents >>= print . checkSum . map readInts . C.lines
--main2 = C.getContents >>= print . checkSum2 . map readInts . C.lines
