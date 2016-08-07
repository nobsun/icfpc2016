
import Control.Concurrent (threadDelay)
import System.IO.Error (tryIOError)
import System.Directory (doesFileExist)

import File (responseFile)
import Solution (genSimpleSolution)
import Command (runCommand)
import ProblemDupes (genDupesMap)


submitSimpleFirst :: Int -> IO ()
submitSimpleFirst n = do
  found  <- doesFileExist (responseFile n)
  if found
    then putStrLn $ "response already exists: " ++ responseFile n
    else do
    genSimpleSolution n
    runCommand "./submit_solution.sh" [show n]
    threadDelay $ 3600 * 1000

main :: IO ()
main = mapM_ (tryIOError . submitSimpleFirst) . map fst . genDupesMap =<< getContents
