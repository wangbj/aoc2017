module Day3 (
            ) where

import qualified Data.Map.Strict as Map
import           Data.Map (Map)
import qualified Data.IntMap.Strict as IntMap
import           Data.IntMap (IntMap)
import           Data.Maybe
import           Data.Tuple

{-

1

4 3
1 2

5 4 3
6 1 2
7 8 9

16 15 14 13
 5  4  3 12
 6  1  2 11
 7  8  9 10

17 16 15 14 13
18  5  4  3 12
19  6  1  2 11
20  7  8  9 10
21 22 23 24 25

-}

cachedSquares = IntMap.fromList . flip zip [1..] . map square . enumFromTo 1 $ 65535
  where square x = x * x

isqrt :: Int -> Int
isqrt = maybe 0 (fromIntegral . snd) . flip IntMap.lookupGE cachedSquares . fromIntegral


-- Instead of move one item at a time, we can also move the sub matrix recursively
-- either move the previous matrix to the bottom left (even)
-- or move the previouis matrix to the top right (odd)
moveDown = Map.fromList . map ( \((r,c), v) -> ( (1+r, c), v)) . Map.assocs
moveRight = Map.fromList . map ( \((r,c),v) -> ( (r, 1+c), v)) . Map.assocs

makePlane :: Int -> Map (Int, Int) Int
makePlane = foldl acc Map.empty . enumFromTo 1
  where acc m k
          | k == 1 = Map.singleton (1,1) 1
          | even k = Map.union (moveDown m) (Map.fromList (firstRow ++ lastCol))
          | odd k  = Map.union (moveRight m) (Map.fromList (lastRow ++ firstCol))
              where
                firstRow = zip (zip (repeat 1) [1..k]) [k*k, k*k-1 ..1]
                lastCol  = zip (zip [1..k] (repeat k)) [k*k-k+1, k*k-k..1]
                lastRow = zip (zip (repeat k) [1..k]) [k*k-k+1..k*k]
                firstCol = zip (zip [1..k] (repeat 1)) [1+(k-1)*(k-1)..k*k-k+1]

makeMap = IntMap.fromList . map swap . Map.assocs . makePlane

distance n = abs (i1-i2) + abs (j1-j2)
  where dict = makeMap (isqrt n)
        (i1,j1) = fromMaybe (0,0) (IntMap.lookup 1 dict)
        (i2,j2) = fromMaybe (0,0) (IntMap.lookup n dict)

-- the idea is similar, but generate filler from adjcents cells (if available)
makePlane2 :: Int -> Map (Int, Int) Int
makePlane2 = foldl acc Map.empty . enumFromTo 1
  where acc m k
          | k == 1 = Map.singleton (1,1) 1
          | even k = foldl fill (foldl fill (moveDown m) (zip [k,k-1..1] (repeat k))) (zip (repeat 1) [k,k-1..1])
          | odd k  = foldl fill (foldl fill (moveRight m) (zip [1..k] (repeat 1))) (zip (repeat k) [1..k])
              where
                fill m (i, j) = Map.insert (i,j) x m
                  where adjs = filter (\(r,c) -> r > 0 && r <= k && c > 0 && c <= k) $
                          [ (i-1, j), (i, j-1), (i+1, j), (i, j+1)
                          , (i+1, j+1), (i+1, j-1), (i-1, j-1), (i-1, j+1) ]
                        x = sum . catMaybes . map (flip Map.lookup m) $ adjs

makeMap2 = IntMap.fromList . map swap . Map.assocs . makePlane2

firstVal n = IntMap.lookupGT n (makeMap2 (isqrt n))
