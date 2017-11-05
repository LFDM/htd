{-# LANGUAGE TemplateHaskell #-}

module ListView where

import qualified Data.Vector as Vec
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Todos
import Todo

import Renderable

import Control.Lens

data TodoListView = TodoListView { label :: String, _list :: L.List () Todo } deriving (Show)
makeLenses ''TodoListView

createListViewState :: Todos -> TodoListView
createListViewState ts = TodoListView { label="Todos", _list=list}
  where list =  L.list () (vec ts) 1
        vec = Vec.fromList . getTodosList

createListView :: TodoListView -> Widget ()
createListView TodoListView { label=l, _list=ls } = C.vCenter $ vBox [ C.hCenter b ]
  where b = B.borderWithLabel (str l ) $ hLimit 100 $ vLimit 20 $ renderedList
        renderedList = L.renderList drawEl True ls
        drawEl sel =  str . render

handleListEvent :: V.Event -> TodoListView -> T.EventM () TodoListView
handleListEvent e v = T.handleEventLensed v list consume e
  where consume = L.handleListEvent -- also needs to handle vi bindings
