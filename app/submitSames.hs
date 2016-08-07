
import Control.Applicative
import Control.Concurrent (threadDelay)
import Data.Bool
import Data.Maybe
import Text.Read (readMaybe)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

import File (solutionFile, problemFile, responseFile)
import Command (runCommand)
import ProblemDupes (genDupesMap)
import REST.Response (SolutionSubmission (..))
import REST.File (loadSolutionSubmission)


submitCopyInterval :: Maybe Int -> Int -> Int -> IO ()
submitCopyInterval sleep orig dest = do
  let probFn = problemFile orig
      solFn  = solutionFile orig

  e1  <-  bool (Just $ "original problem file not found: " ++ probFn) Nothing <$> doesFileExist probFn
  e2  <-  bool (Just $ "original solution file not found: " ++ solFn) Nothing <$> doesFileExist solFn

  let respFn = responseFile dest
      cleared ss
        | abs (1.0 - solutionSubmission_resemblance ss) < 0.000001  =  Just $ "have cleared result: " ++ respFn
        | otherwise                                                 =  Nothing

  eResp <- doesFileExist respFn
  clear <- if eResp
           then (cleared =<<) <$> loadSolutionSubmission dest
           else return Nothing

  case (e1 <|> e2 <|> clear) of
    Nothing  -> do
      runCommand "./submit_solution.sh" [show dest, show orig]
      threadDelay $ maybe (3600 * 1000) (* 1000) sleep
    Just msg ->
      putStrLn msg

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
