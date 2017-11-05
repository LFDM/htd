{-# LANGUAGE TemplateHaskell #-}

module Todos where

import Todo
import Control.Lens
import Data.Map as Map
import Data.List as List

data Todos = Todos { _todos :: Map String Todo
                   , _path :: FilePath
                   } deriving (Show)

makeLenses ''Todos

createTodosContainer :: FilePath -> [Todo] -> Todos
createTodosContainer p ts = Todos{_todos=indexById ts, _path=p}

getTodosList :: Todos -> [Todo]
getTodosList = elems . _todos

updateTodos :: Todos -> [Todo] -> Todos
updateTodos c ts = todos .~ (Map.union newTodos oldTodos) $ c
  where newTodos = indexById ts
        oldTodos = _todos c

indexById :: [Todo] -> Map String Todo
indexById = fromList . List.map (\t -> (Todo.id t, t))

isEmpty :: Todos -> Bool
isEmpty = Map.null . _todos

