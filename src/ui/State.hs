{-# LANGUAGE TemplateHaskell #-}


module State where

import Control.Lens
import Todos
import ListView

data Screen = WELCOME | TODOS deriving (Show)

data State = State { _screen :: Screen, _currentTodos :: Todos, _todoList :: TodoListView }
  deriving (Show)

makeLenses ''State

