{-# LANGUAGE OverloadedStrings #-}

module Run where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Data.Vector as Vec
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
import Brick.Util (fg, on)
import Control.Lens

import Util

import Register
import State
import Style

import Todos
import Todo
import ReadWrite
import Renderable

import TitleView
import ListView
import Editor

data TodoList = TodoList { label :: String, list :: L.List String Todo } deriving (Show)

type MainUi = String

handleEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
handleEvent s@State{_mode=mode} e = case mode of
                                      TODOS -> handleEventInListMode s e
                                      TODO_EDIT -> handleEventInEditMode updateSelectedTodoFromEditor s e
                                      TODO_ADD -> handleEventInEditMode Prelude.id s e


handleEventInListMode :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
handleEventInListMode s (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt s
    V.EvKey (V.KChar 'q') []        -> M.halt s
    V.EvKey V.KEnter []      -> persistAndContinue (syncTodos . toggleTodoStatus) s
    V.EvKey (V.KChar ' ') [] -> persistAndContinue (syncTodos . toggleTodoStatus) s
    V.EvKey (V.KChar 'i') [] -> M.continue $ goToEditMode moveToStart s
    V.EvKey (V.KChar 'c') [] -> M.continue $ goToEditMode moveToStart s
    V.EvKey (V.KChar 'e') [] -> M.continue $ goToEditMode moveToEnd s
    V.EvKey (V.KChar 'a') [] -> M.continue $ goToEditMode moveToEnd s
    V.EvKey (V.KChar 'd') [] -> persistAndContinue (syncTodos . removeSelectedTodo) s
    _ -> M.continue =<< T.handleEventLensed s todoList handleListEvent e
handleEventInListMode  s _ = M.continue s

handleEventInEditMode :: (State -> State) -> State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
handleEventInEditMode onSubmit s (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'c') [V.MCtrl] -> M.continue (goToListMode s)
    V.EvKey V.KEsc [] -> M.continue (goToListMode s)
    V.EvKey V.KEnter [] -> persistAndContinue (goToListMode . syncTodos . onSubmit ) s
    _ -> M.continue =<< T.handleEventLensed s editor handleEditorEvent e
handleEventInEditMode  _ s _ = M.continue s

removeSelectedTodo :: State -> State
removeSelectedTodo = withLens todoList removeSelectedItem

goToListMode :: State -> State
goToListMode = set mode TODOS

goToEditMode :: (Editor -> Editor) -> State -> State
goToEditMode postProcessEditor s = tryToGoToEditMode selectedItem
  where selectedItem = (getSelectedListItem . view todoList) s
        tryToGoToEditMode Nothing = s
        tryToGoToEditMode (Just el) = startEditor TODO_EDIT postProcessEditor el s

startEditor :: Mode -> (Editor -> Editor) -> Todo -> State -> State
startEditor m postProcess todo = setEditMode . setupEditor
  where setEditMode = set mode m
        setupEditor = set editor (makeEditor todo)
        makeEditor = postProcess . createEditor . view title

setMode :: Mode -> State -> State
setMode = set mode

updateSelectedTodoFromEditor :: State -> State
updateSelectedTodoFromEditor s = withLens todoList (updateSelectedItem updateTitle) s
  where nextTitle = getEditorText $ view editor s
        updateTitle = set title nextTitle

toggleTodoStatus :: State -> State
toggleTodoStatus = withLens todoList toggleSelectedItemStatus

syncTodos :: State -> State
syncTodos s = withLens currentTodos nextContainer s
  where nextContainer ts = updateTodos ts nextList
        nextList = getListItems . view todoList $ s

persistAndContinue :: (State -> State) -> State -> T.EventM Name (T.Next State)
persistAndContinue f s = M.continue =<< liftIO (persist (f s))

persist :: State -> IO State
persist s = do
  writeTodoFile $ view currentTodos s
  return s

drawUi :: State -> [Widget Name]
drawUi s = [vBox (widgets (view mode s))]
  where titleView = createTitleView s
        todoView  = createListView . view todoList $ s
        helpView = str "Some help text"
        widgets TODOS = [ titleView, todoView ]
        widgets TODO_EDIT = [ titleView, todoView, editorView "Edit Todo: " s ]
        widgets _ = [ titleView, helpView ]

editorView :: String -> State -> Widget Name
editorView p s = pretext <+> editView s
  where pretext = withAttr preTextAttr $ str p
        editView = createEditorView . view editor


app :: M.App State e Name
app = M.App { M.appDraw = drawUi
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = handleEvent
            , M.appStartEvent = return
            , M.appAttrMap = const styleMap
  }

determineIntitialMode :: Todos -> Mode
determineIntitialMode ts | isEmpty ts = WELCOME
determineIntitialMode ts = TODOS

createInitialState :: IO State
createInitialState = do
  -- read a general config
  todoContainer <- readTodoList
  return State { _currentTodos=todoContainer
               , _todoList=createListViewState todoContainer
               , _mode=determineIntitialMode todoContainer
               , _editor=emptyEditor
               }

runApp :: [String] -> IO ()
runApp _ = void $ createInitialState >>= M.defaultMain app
