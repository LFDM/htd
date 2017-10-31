{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Todo (
  Todo
) where

import GHC.Generics

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), ToJSON(..))
import qualified Data.ByteString.Char8 as BS

type TodoStatus = String

data Todo = Todo { title :: String
                 , status :: TodoStatus
                 } deriving (Show, Generic)

instance FromJSON Todo
instance ToJSON Todo
