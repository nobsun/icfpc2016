module ProblemDupes where

import Control.Monad
import Data.Maybe
import Data.List (stripPrefix, isSuffixOf, sort)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)


takeProblemNum :: String -> Maybe Int
takeProblemNum path = do
  fn  <-  stripPrefix "problems/" path
  let suf = ".dat"
  guard $ suf `isSuffixOf` fn
  readMaybe $ take (length fn - length suf) fn

uncons :: [a] -> Maybe (a, [a])
uncons (x:xs) = Just (x, xs)
uncons  []    = Nothing

genDupesMap :: String -> [(Int, [Int])]
genDupesMap =
  filter (not . null)
  . mapMaybe (uncons . sort . mapMaybe takeProblemNum)
  . splitOn [""] . lines
