{-# LANGUAGE TemplateHaskell #-}


module State where

import Control.Lens
import Todos
import ListView
import Editor
import qualified Brick.Focus as F


data Mode = WELCOME | TODOS | TODO_ADD | TODO_EDIT deriving (Show)

data State = State
  { _mode :: Mode
  , _currentTodos :: Todos
  , _todoList :: TodoListView
  , _editor :: Editor
  } deriving (Show)

makeLenses ''State

