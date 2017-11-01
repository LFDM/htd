{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo where

import GHC.Generics

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), ToJSON(..))
import qualified Data.ByteString.Char8 as BS
import Control.Lens


data TodoStatus = DONE | NOT_DONE | ARCHIVED
  deriving (Show, Generic)
instance FromJSON TodoStatus
instance ToJSON TodoStatus

data Todo = Todo { _title :: String
                 , _status :: TodoStatus
                 , id :: String
                 } deriving (Show, Generic)

instance FromJSON Todo
instance ToJSON Todo

makeLenses ''Todo
