{-# LANGUAGE OverloadedStrings #-}

module Run where

import Control.Monad (void)
import Data.Monoid
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
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

import Todos
import Todo
import ReadWrite
import Renderable

import ListView

data State = State { _screen :: Screen, _currentTodos :: Todos, _todoList :: TodoListView }
  deriving (Show)

data TodoList = TodoList { label :: String, list :: L.List String Todo } deriving (Show)

data Screen = WELCOME | TODOS deriving (Show)

type MainUi = String

attrMap :: A.AttrMap
attrMap = A.attrMap V.defAttr []

appEvent :: State -> T.BrickEvent () e -> T.EventM n (T.Next State)
appEvent s _ = M.continue s

drawUi :: State -> [Widget ()]
drawUi s = [todoList]
  where todoList = createListView (_todoList s)

app :: M.App State e ()
app = M.App { M.appDraw = drawUi
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = return
            , M.appAttrMap = const attrMap
  }

determineIntitialScreen :: Todos -> Screen
determineIntitialScreen ts | isEmpty ts = WELCOME
determineIntitialScreen ts = TODOS


createInitialState :: IO State
createInitialState = do
  -- read a general config
  todoContainer <- readTodoList
  return State { _currentTodos=todoContainer
               , _todoList=createListViewState todoContainer
               , _screen=determineIntitialScreen todoContainer
               }

runApp :: [String] -> IO ()
runApp _ = void $ createInitialState >>= M.defaultMain app
