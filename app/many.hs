
import Data.List
import Data.Function
import ProblemDupes (genDupesMap)

main :: IO ()
main =
  mapM_ print . sortBy (compare `on` length . snd) . genDupesMap =<< getContents
