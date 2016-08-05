{-# LANGUAGE DeriveGeneric #-}

module REST.Response (
  Hello (..),
  StatusSnapshot (..), Snapshot (..),
  ProblemSubmission (..),
  SolutionSubmission (..),
  ) where

import GHC.Generics (Generic)
import Data.Word (Word64)
import Data.Maybe
import Data.List (stripPrefix)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson (ToJSON, FromJSON, genericToJSON, genericParseJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson


type Timestamp = Word64

stripPrefix' :: Eq a => [a] -> [a] -> [a]
stripPrefix' pre s =
  fromMaybe s $ stripPrefix pre s


data Hello =
  Hello
  { hello_ok        ::  Bool
  , hello_greeting  ::  String
  } deriving (Eq, Show, Generic)

instance ToJSON Hello where
  toJSON =
    genericToJSON
    Aeson.defaultOptions
    { Aeson.fieldLabelModifier = stripPrefix' "hello_" }

instance FromJSON Hello where
  parseJSON =
    genericParseJSON
    Aeson.defaultOptions
    { Aeson.fieldLabelModifier = stripPrefix' "hello_" }

_t_hello :: (L8.ByteString, Maybe Hello)
_t_hello =
  (Aeson.encode $ Hello True "Hello",
   Aeson.decode $ L8.pack "{\"ok\":true,\"greeting\":\"Hello\"}")


data Snapshot =
  Snapshot
  { snapshot_time  ::  Timestamp
  , snapshot_hash  ::  String
  } deriving (Eq, Show, Generic)

instance ToJSON Snapshot where
  toJSON = genericToJSON Aeson.defaultOptions

instance FromJSON Snapshot where
  parseJSON = genericParseJSON Aeson.defaultOptions

data StatusSnapshot =
  StatusSnapshot
  { statusSnapshot_ok         ::  Bool
  , statusSnapshot_snapshots  ::  [Snapshot]
  } deriving (Eq, Show, Generic)

instance ToJSON StatusSnapshot where
  toJSON =
    genericToJSON
    Aeson.defaultOptions
    { Aeson.fieldLabelModifier = stripPrefix' "statusSnapshot_" }

instance FromJSON StatusSnapshot where
  parseJSON =
    genericParseJSON
    Aeson.defaultOptions
    { Aeson.fieldLabelModifier = stripPrefix' "statusSnapshot_" }

_t_statusSnapshot :: (L8.ByteString, Maybe StatusSnapshot)
_t_statusSnapshot =
  (Aeson.encode $
   StatusSnapshot
   { statusSnapshot_ok = True
   , statusSnapshot_snapshots =
     [ Snapshot 1470355200 "0123456789abcdef0123456789abcdef"
     , Snapshot 1470358800 "fedcba9876543210fedcba9876543210" ]},
   Aeson.decode $
   L8.pack $ concat
   [ "{"
   , "  \"ok\": true,"
   , "  \"snapshots\": ["
   , "    {"
   , "      \"snapshot_time\": 1470355200,"
   , "      \"snapshot_hash\": \"0123456789abcdef0123456789abcdef\""
   , "    },"
   , "    {"
   , "      \"snapshot_time\": 1470358800,"
   , "      \"snapshot_hash\": \"fedcba9876543210fedcba9876543210\""
   , "    }"
   , "  ]"
   , "}"
   ])


data ProblemSubmission =
  ProblemSubmission
  { problemSubmission_ok                  ::  Bool
  , problemSubmission_problem_id          ::  Int
  , problemSubmission_publish_time        ::  Timestamp
  , problemSubmission_solution_spec_hash  ::  String
  , problemSubmission_solution_size       ::  Int
  , problemSubmission_problem_spec_hash   ::  String
  , problemSubmission_problem_size        ::  Int
  } deriving (Eq, Show, Generic)

instance ToJSON ProblemSubmission where
  toJSON =
    genericToJSON
    Aeson.defaultOptions
    { Aeson.fieldLabelModifier = stripPrefix' "problemSubmission_" }

instance FromJSON ProblemSubmission where
  parseJSON =
    genericParseJSON
    Aeson.defaultOptions
    { Aeson.fieldLabelModifier = stripPrefix' "problemSubmission_" }

_t_problemSubmission :: Maybe ProblemSubmission
_t_problemSubmission =
  Aeson.decode . L8.pack $ concat
  [ "{"
  , "  \"ok\": true,"
  , "  \"problem_id\": 4,"
  , "  \"publish_time\": 1475280000,"
  , "  \"solution_spec_hash\": \"27e3c42fa46aec6fcf438bb5c326d55e27c91811\","
  , "  \"solution_size\": 78,"
  , "  \"problem_spec_hash\": \"f4b1a8567108144bae331340a57c68b85df487e0\","
  , "  \"problem_size\": 67"
  , "}"
  ]


data SolutionSubmission =
  SolutionSubmission
  { solutionSubmission_ok                  ::  Bool
  , solutionSubmission_problem_id          ::  Int
  , solutionSubmission_resemblance         ::  Double
  , solutionSubmission_solution_spec_hash  ::  String
  , solutionSubmission_solution_size       ::  Int
  } deriving (Eq, Show, Generic)

instance ToJSON SolutionSubmission where
  toJSON =
    genericToJSON
    Aeson.defaultOptions
    { Aeson.fieldLabelModifier = stripPrefix' "solutionSubmission_" }

instance FromJSON SolutionSubmission where
  parseJSON =
    genericParseJSON
    Aeson.defaultOptions
    { Aeson.fieldLabelModifier = stripPrefix' "solutionSubmission_" }

_t_solutionSubmission :: Maybe SolutionSubmission
_t_solutionSubmission =
  Aeson.decode . L8.pack $ concat
  [ "{"
  , "  \"ok\": true,"
  , "  \"problem_id\": 1,"
  , "  \"resemblance\": 1.0,"
  , "  \"solution_spec_hash\": \"27e3c42fa46aec6fcf438bb5c326d55e27c91811\","
  , "  \"solution_size\": 78"
  , "}"
  ]
