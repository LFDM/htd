{-# LANGUAGE TemplateHaskell #-}

module ListView
( TodoListView
, createListViewState
, createListView
, handleListEvent
, getSelectedListItem
, toggleSelectedItemStatus
, updateSelectedItem
, removeSelectedItem
, insertBeforeSelection
, insertBehindSelection
, moveSelectedItem
, getListItems
) where

import Data.Maybe
import qualified Data.Vector as Vec
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Brick.Types
  ( Widget
  )
import Brick.Util (clamp)
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
import Util

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
getSelectedListItem  = fmap snd . getSelection

getSelectedIdx :: TodoListView -> Maybe Int
getSelectedIdx = fmap fst . getSelection

getSelection :: TodoListView -> Maybe (Int, Todo)
getSelection TodoListView { _list=ls } = L.listSelectedElement ls

hasSelection :: TodoListView -> Bool
hasSelection = check . getSelectedListItem
  where check Nothing = False
        check (Just _) = True

updateSelectedItem :: (Todo -> Todo) -> TodoListView -> TodoListView
updateSelectedItem f = withLens list (L.listModify f)

removeSelectedItem :: TodoListView -> TodoListView
removeSelectedItem v = tryRemove . getSelectedIdx $ v
  where tryRemove Nothing = v
        tryRemove (Just i) = withLens list (L.listRemove i) v

toggleSelectedItemStatus :: TodoListView -> TodoListView
toggleSelectedItemStatus ls = updateList selected
  where selected = getSelectedListItem ls
        updateList Nothing = ls
        updateList (Just el) = updateSelectedItem (mark (getNextStatus el)) ls
        getNextStatus t = if (view status t == DONE) then NOT_DONE else DONE

getListItems :: TodoListView -> [Todo]
getListItems = Vec.toList . view (list . L.listElementsL)

getListLength :: TodoListView -> Int
getListLength = Vec.length . view (list . L.listElementsL)

insertBeforeSelection :: Todo -> TodoListView -> TodoListView
insertBeforeSelection = insertAroundSelection 0

insertBehindSelection :: Todo -> TodoListView -> TodoListView
insertBehindSelection = insertAroundSelection 1

insertAroundSelection :: Int -> Todo -> TodoListView -> TodoListView
insertAroundSelection offset t v = moveSelectionRel offset $ withLens list (L.listInsert pos t) v
  where pos = (fromMaybe 0 (getSelectedIdx v)) + offset

moveSelectionRel :: Int -> TodoListView -> TodoListView
moveSelectionRel offset v = update (getSelectedIdx v) v
  where update Nothing s = s
        update (Just i) s = moveSelectionAbs (i + offset) s

moveSelectionAbs :: Int -> TodoListView -> TodoListView
moveSelectionAbs idx = withLens list (L.listMoveTo idx)

moveSelectedItem :: Int -> TodoListView -> TodoListView
moveSelectedItem offset v = move pos
  where pos = getSelectedIdx v
        move Nothing = v
        move (Just i) = switchItems i (i + offset) v

switchItems :: Int -> Int -> TodoListView -> TodoListView
switchItems x y v = if x /= y && isWithinBounds x v && isWithinBounds y v
                      then withLens list replace v
                      else v
  where a = if x < y then x else y
        b = if x < y then y else x
        isWithinBounds i v = i >= 0 && i <= (getListLength v) - 1
        replace l = L.listReplace ((safeSwitch a b. (view L.listElementsL)) l) (Just y) l

safeSwitch :: Int -> Int -> Vec.Vector a -> Vec.Vector a
safeSwitch x y v = Vec.concat [before, elY, inBetween, elX, after]
  where before = Vec.take x v
        elY = Vec.fromList [v Vec.! y]
        inBetween = Vec.slice (x + 1) (y - (x + 1)) v
        elX = Vec.fromList [v Vec.! x]
        after = Vec.drop (y + 1) v

