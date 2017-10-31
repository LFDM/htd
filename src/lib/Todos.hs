module Todos (
  Todos,
  newTodos
) where

import Todo

data Todos = Todos { todos :: [Todo]
                   , path :: FilePath
                   } deriving (Show)

newTodos :: [Todo] -> FilePath -> Todos
newTodos ts p = Todos { todos=ts, path=p }


