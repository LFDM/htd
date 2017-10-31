{-# LANGUAGE OverloadedStrings #-}

module Todo (
  Todo
) where
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?))
import qualified Data.ByteString.Char8 as BS

type TodoStatus = String

data Todo = Todo { title :: String
                 , status :: TodoStatus
                 } deriving (Show)

instance FromJSON Todo where
  parseJSON (Y.Object v) =
    Todo <$>
      v .: "title" <*>
      v .: "status"

