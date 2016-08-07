module REST.File where

import Data.Bool (bool)
import System.Directory (doesFileExist)

import File (responseFile)
import REST.Response
  (BlobSnapshot (..), Problem (..), SolutionSubmission (..))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LB


loadSnapshot :: IO (Maybe BlobSnapshot)
loadSnapshot = Aeson.decode <$> LB.readFile "bloblookup.json"

sampouTeamId :: String
sampouTeamId = "49"

getOwnProblemNums :: BlobSnapshot -> [Int]
getOwnProblemNums  = map problem_problem_id . filter ((== sampouTeamId)  . problem_owner) . blobSnapshot_problems

loadSolutionSubmission :: Int -> IO (Maybe SolutionSubmission)
loadSolutionSubmission n = do
  let fn = responseFile n
  bool (return Nothing) (Aeson.decode <$> LB.readFile fn) =<< doesFileExist fn
