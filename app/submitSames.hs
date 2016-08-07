
import Control.Applicative
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.Bool
import Data.Maybe
import Data.List (stripPrefix, isSuffixOf, sort)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

import File (solutionFile, problemFile)
import Command (runCommand)


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

submitCopyInterval :: Maybe Int -> Int -> Int -> IO ()
submitCopyInterval sleep orig dest = do
  let probFn = problemFile orig
      solFn  = solutionFile orig

  e1  <-  bool (Just $ "original problem file not found: " ++ probFn) Nothing <$> doesFileExist probFn
  e2  <-  bool (Just $ "original solution file not found: " ++ solFn) Nothing <$> doesFileExist solFn

  case (e1 <|> e2) of
    Nothing  -> do
      runCommand "./submit_solution.sh" [show dest, show orig]
      threadDelay $ maybe (3600 * 1000) (* 1000) sleep
    Just err ->
      putStrLn err

submitCopies :: Maybe Int -> (Int, [Int]) -> IO ()
submitCopies sleep (orig, cs) = mapM_ (submitCopyInterval sleep orig) cs

--

main :: IO ()
main = do
  mayArg <- listToMaybe <$> getArgs
  sleep  <-  case mayArg of
    Just arg ->  maybe (fail "only sleep mili-second allowed") (return . Just) $ readMaybe arg
    Nothing  ->  return Nothing

  dupes <- genDupesMap <$> getContents
  mapM_ print dupes
  mapM_ (submitCopies sleep) dupes
