
import Data.List
import Data.Function
import qualified Data.Set as Set
import ProblemDupes (genDupesMap)
import System.Process (readProcess)

data Clear
  = Clear
  | Not__
  deriving (Eq, Show)

main :: IO ()
main = do
  clearSet  <-  Set.fromList . map read . lines <$> readFile "CLEAR.md"
  let ok True   = Clear
      ok False  = Not__
      record (orig, cs) = (ok $ orig `Set.member` clearSet, (orig, length cs))
  mapM_ print . sortBy (compare `on` snd . snd) . map record . genDupesMap
    =<< readProcess "fdupes" ["-r", "problems"] ""
