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

import Register
import State
import Style

import Todos
import Todo
import ReadWrite
import Renderable

import TitleView
import ListView

data TodoList = TodoList { label :: String, list :: L.List String Todo } deriving (Show)

type MainUi = String

handleEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
handleEvent s (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt s
handleEvent s (T.VtyEvent (V.EvKey V.KEnter [])) = persistAndContinue' (syncTodos . toggleTodoStatus) s
handleEvent s (T.VtyEvent e) = M.continue =<< T.handleEventLensed s todoList handleListEvent e
handleEvent s _ = M.continue s

toggleTodoStatus :: State -> State
toggleTodoStatus s = set todoList (toggle s) s
  where toggle = toggleSelectedItemStatus . view todoList

syncTodos :: State -> State
syncTodos s = set currentTodos nextContainer s
  where nextContainer = updateTodos (view currentTodos s) nextList
        nextList = getListItems . view todoList $ s

persistAndContinue' :: (State -> State) -> State -> T.EventM Name (T.Next State)
persistAndContinue' f s = persistAndContinue (f s)

persistAndContinue :: State -> T.EventM Name (T.Next State)
persistAndContinue s = M.continue =<< liftIO (persist s)

persist :: State -> IO State
persist s = do
  writeTodoFile $ view currentTodos s
  return s

drawUi :: State -> [Widget Name]
drawUi s = [vBox [ titleView, todoView ]]
  where titleView = createTitleView s
        todoView  = createListView . view todoList $ s

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
               }

runApp :: [String] -> IO ()
runApp _ = void $ createInitialState >>= M.defaultMain app
