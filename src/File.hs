module File where

import System.FilePath


solutionFile :: Int -> FilePath
solutionFile n = "answers" </> show n <.> "dat"

problemFile :: Int -> FilePath
problemFile n = "problems" </> show n <.> "dat"

responseFile :: Int -> FilePath
responseFile n = "responses" </> show n <.> "json"
