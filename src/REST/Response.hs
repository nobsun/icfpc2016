{-# LANGUAGE DeriveGeneric #-}

module REST.Response (
  Hello (..),
  StatusSnapshot (..), Snapshot (..),
  ) where

import GHC.Generics (Generic)
import Data.Word (Word64)
import Data.Maybe
import Data.List (stripPrefix)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson (ToJSON, FromJSON, genericToJSON, genericParseJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson



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
  { snapshot_time  ::  Word64
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
   L8.pack "{\"ok\":true,\"snapshots\":[{\"snapshot_time\":1470355200,\"snapshot_hash\":\"0123456789abcdef0123456789abcdef\"},{\"snapshot_time\":1470358800,\"snapshot_hash\":\"fedcba9876543210fedcba9876543210\"}]}")
