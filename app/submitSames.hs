
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.Maybe
import Data.List (stripPrefix, isSuffixOf, sort)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import System.FilePath
import System.Directory (doesFileExist)
import System.Process (rawSystem)
import System.Exit
import System.Environment (getArgs)


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

-- solutionFile n = "answers" </> show n <.> "dat"

problemFile :: Int -> FilePath
problemFile n = "problems" </> show n <.> "dat"

runCommand :: String -> [String] -> IO ()
runCommand cmd args = do
  ec <- rawSystem cmd args
  case ec of
    ExitSuccess     -> return ()
    ExitFailure en  -> putStrLn $ "Error exit with " ++ show en ++ ": " ++ unwords (cmd : args)

submitCopyInterval :: Maybe Int -> Int -> Int -> IO ()
submitCopyInterval sleep orig dest = do
  let origFn = problemFile orig
  origE <- doesFileExist origFn
  if origE
    then do runCommand "./submit_solution.sh" [show dest, show orig]
            threadDelay $ maybe (3600 * 1000) (* 1000) sleep
    else    putStrLn $ "original file not found: " ++ origFn

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
