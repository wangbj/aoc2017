import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import qualified Data.IntMap.Strict as IntMap
import           Data.IntMap (IntMap)
import           Data.Maybe
import           Data.List (unfoldr)
import           Data.Char

-- IntMap provides O(1) for most operations, including indexing, update
-- althoug it is slower than MVector but a lot easier to use
countSteps l = fst . until (outside . fst . snd) iter $ (0,(1,array))
  where
    outside x = x < 1 || x > arraySize
    array = IntMap.fromList (zip [1..] l)
    arraySize = IntMap.size array
    iter (n,(k,m)) | outside k = (n,(k,m))
                   | otherwise = (1+n, (k + v, IntMap.update (Just . succ) k m))
                   where v = fromMaybe 0 (IntMap.lookup k m)

countSteps2 l = fst . until (outside . fst . snd) iter $ (0,(1,array))
  where
    outside x = x < 1 || x > arraySize
    array = IntMap.fromList (zip [1..] l)
    arraySize = IntMap.size array
    iter (n,(k,m)) | outside k = (n,(k,m))
                   | otherwise = (1+n, (k + v, IntMap.update (Just . incDec) k m))
                   where v = fromMaybe 0 (IntMap.lookup k m)
                         incDec x = if x >= 3 then x - 1 else x + 1

readints = unfoldr (fmap (fmap (C.dropWhile isSpace)) . C.readInt)

main = C.getContents >>= print . countSteps . readints
--main = C.getContents >>= print . countSteps2 . readints
