{-# LANGUAGE TemplateHaskell #-}

module Todos where

import Todo
import Control.Lens

data Todos = Todos { _todos :: [Todo]
                   , _path :: FilePath
                   } deriving (Show)

makeLenses ''Todos

