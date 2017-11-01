{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo where

import GHC.Generics

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), ToJSON(..))
import qualified Data.ByteString.Char8 as BS
import Control.Lens

type TodoStatus = String

data Todo = Todo { _title :: String
                 , _status :: TodoStatus
                 } deriving (Show, Generic)

instance FromJSON Todo
instance ToJSON Todo

makeLenses ''Todo
