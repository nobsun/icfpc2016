module Command where

import System.Process (rawSystem)
import System.Exit


runCommand :: String -> [String] -> IO ()
runCommand cmd args = do
  ec <- rawSystem cmd args
  case ec of
    ExitSuccess     -> return ()
    ExitFailure en  -> putStrLn $ "Error exit with " ++ show en ++ ": " ++ unwords (cmd : args)
