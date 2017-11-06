{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo where

import GHC.Generics


import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), ToJSON(..))
import qualified Data.ByteString.Char8 as BS
import Control.Lens
import Renderable
import Util


data TodoStatus = DONE | NOT_DONE | ARCHIVED
  deriving (Show, Generic, Eq)
instance FromJSON TodoStatus
instance ToJSON TodoStatus
instance Renderable TodoStatus where
  render DONE = "[✔]"
  render NOT_DONE = "[ ]"
  render ARCHIVED  = "---"

data Todo = Todo { _title :: String
                 , _status :: TodoStatus
                 , id :: String
                 } deriving (Show, Generic)

instance FromJSON Todo
instance ToJSON Todo
instance Renderable Todo where
  render t = unwords [render (_status t), _title t]

makeLenses ''Todo

mark :: TodoStatus -> Todo -> Todo
mark s = status .~ s

createNewTodo :: IO Todo
createNewTodo = do
  nextId <- generateId
  return Todo { _title="", _status=NOT_DONE, Todo.id=nextId}

