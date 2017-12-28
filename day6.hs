import qualified Data.ByteString.Char8 as C
import           Data.ByteString (ByteString)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.IntMap.Strict as IntMap
import           Data.IntMap (IntMap)
import           Data.List
import           Data.Tuple
import           Data.Maybe
import           Data.Char

input :: ByteString
input = C.pack "2 8 8 5 4 2 3 1 5 5 1 2 15 13 5 14"

hash :: IntMap Int -> Integer
hash = IntMap.foldl acc 0
  where acc l x = l * 256 + fromIntegral x

readints = unfoldr (fmap (fmap (C.dropWhile isSpace)) . C.readInt)

maxKV = swap . IntMap.findMax . IntMap.fromListWith (flip const) . map swap . IntMap.assocs

redistribute :: Int -> IntMap Int -> IntMap Int
redistribute size m = redist (1 + maxK `mod` size, maxV) m'
  where
    (maxK, maxV) = maxKV m
    m'           = IntMap.update (const (Just 0)) maxK m
    redist (k,v) m
      | v == 0 = m
      | v  > 0 = redist (k', pred v) (IntMap.update (Just . succ) k m)
      where k' = 1 + k `mod` size

helper l = until (\(n, (s, m)) -> hash m `Set.member` s) go $ (0, (Set.empty, IntMap.fromList (zip [1..] l)))
  where
    size = length l
    go  (n, (s, m)) = (1+n, (Set.insert (hash m) s, m'))
      where m' = redistribute size m

balance = fst . helper
balance2 = balance . map snd . IntMap.toList . snd . snd . helper
