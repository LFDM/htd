{-# LANGUAGE TemplateHaskell #-}


module State where

import Control.Lens
import Todos
import ListView

data Mode = WELCOME | TODOS deriving (Show)

data State = State { _mode :: Mode, _currentTodos :: Todos, _todoList :: TodoListView }
  deriving (Show)

makeLenses ''State

