{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import qualified Data.HashSet as HashSet
import           Data.HashSet (HashSet)
import           Control.Monad
import           Data.Maybe
import           Data.List (sort)

-- Yes, it isn't uncommon imports have more lines!
haveDuplicates :: [ByteString] -> Bool
haveDuplicates = isNothing . foldM ins_ HashSet.empty
  where ins_ m s = if s `HashSet.member` m then Nothing else Just (HashSet.insert s m)

validPassphere = not . haveDuplicates . C.words

sortBS = C.pack . sort . C.unpack
validPassphere2 = not . haveDuplicates . map sortBS . C.words

main :: IO ()
main = C.getContents >>= print . length . filter (==True) . map validPassphere . C.lines
-- main = C.getContents >>= print . length . filter (==True) . map validPassphere2 . C.lines
