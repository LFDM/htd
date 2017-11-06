{-# LANGUAGE TemplateHaskell #-}


module State where

import Control.Lens
import Todos
import ListView
import Editor
import qualified Brick.Focus as F


data Mode = WELCOME | TODOS | TODO_ADD_BEFORE | TODO_ADD_BEHIND | TODO_EDIT deriving (Show, Eq)

data State = State
  { _mode :: Mode
  , _currentTodos :: Todos
  , _todoList :: TodoListView
  , _editor :: Editor
  } deriving (Show)

makeLenses ''State

