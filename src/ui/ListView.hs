{-# LANGUAGE TemplateHaskell #-}

module ListView
( TodoListView
, createListViewState
, createListView
, handleListEvent
, markSelectedItem
, getSelectedListItem
, toggleSelectedItemStatus
, getListItems
) where

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
import Register
import Todos
import Todo

import Renderable

import Control.Lens

data TodoListView = TodoListView { label :: String, _list :: L.List Name Todo } deriving (Show)
makeLenses ''TodoListView

maxColumns  = 100
maxRows  = 15

createListViewState :: Todos -> TodoListView
createListViewState ts = TodoListView { label="Todos", _list=list}
  where list =  L.list TODO_LIST (vec ts) 1
        vec = Vec.fromList . getTodosList

createListView :: TodoListView -> Widget Name
createListView TodoListView { label=l, _list=ls } = C.vCenter $ vBox [ C.hCenter b ]
  where b = B.borderWithLabel (str l ) $ hLimit maxColumns $ vLimit maxRows $ renderedList
        renderedList = L.renderList drawEl True ls
        drawEl sel =  str . render

handleListEvent :: V.Event -> TodoListView -> T.EventM Name TodoListView
handleListEvent e v = T.handleEventLensed v list consume e
  where consume = L.handleListEventVi L.handleListEvent

getSelectedListItem :: TodoListView -> Maybe Todo
getSelectedListItem TodoListView { _list=ls } = getEl . L.listSelectedElement $ ls
  where getEl Nothing = Nothing
        getEl (Just (_, todo)) = Just todo

hasSelection :: TodoListView -> Bool
hasSelection = check . getSelectedListItem
  where check Nothing = False
        check (Just _) = True

markSelectedItem :: TodoStatus -> TodoListView -> TodoListView
markSelectedItem s v =  set list modifiedList v
  where modifiedList = L.listModify (mark s) $ view list v

toggleSelectedItemStatus :: TodoListView -> TodoListView
toggleSelectedItemStatus ls = updateList selected
  where selected = getSelectedListItem ls
        updateList Nothing = ls
        updateList (Just el) = markSelectedItem (getNextStatus el) ls
        getNextStatus t = if (view status t == DONE) then NOT_DONE else DONE

getListItems :: TodoListView -> [Todo]
getListItems = Vec.toList . view (list . L.listElementsL)

