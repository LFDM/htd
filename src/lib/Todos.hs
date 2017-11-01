{-# LANGUAGE TemplateHaskell #-}

module Todos where

import Todo
import Control.Lens
import Data.Map
import Data.List as List

data Todos = Todos { _todos :: Map String Todo
                   , _path :: FilePath
                   } deriving (Show)

createTodosContainer :: FilePath -> [Todo] -> Todos
createTodosContainer p ts = Todos{_todos=indexById ts, _path=p}
  where indexById = fromList . List.map (\t -> (Todo.id t, t))

getTodosList :: Todos -> [Todo]
getTodosList = elems . _todos

makeLenses ''Todos

