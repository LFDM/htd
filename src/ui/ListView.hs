module ListView where

import qualified Data.Vector as Vec
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
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

data TodoListView = TodoListView { label :: String, list :: L.List () Todo } deriving (Show)

createListViewState :: Todos -> TodoListView
createListViewState ts = TodoListView { label="", list=list}
  where list =  L.list () (vec ts) 1
        vec = Vec.fromList . getTodosList

createListView :: TodoListView -> Widget ()
createListView TodoListView { label=l, list=ls } = C.vCenter $ vBox [ C.hCenter b ]
  where b = B.borderWithLabel (str l ) $ hLimit 100 $ vLimit 20 $ renderedList
        renderedList = L.renderList drawEl True ls
        drawEl sel =  C.hCenter . str . render
