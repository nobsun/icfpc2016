
import Data.List
import Data.Function
import qualified Data.Set as Set
import ProblemDupes (genDupesMap)
import System.Process (readProcess)

main :: IO ()
main = do
  clearSet  <-  Set.fromList . map read . lines <$> readFile "CLEAR.md"
  mapM_ print . sortBy (compare `on` length . snd) . filter ((`Set.notMember` clearSet) . fst) . genDupesMap
    =<< readProcess "fdupes" ["-r", "problems"] ""
