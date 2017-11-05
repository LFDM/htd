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
import Control.Lens

import State
import Todos
import Todo
import ReadWrite
import Renderable

import ListView

data TodoList = TodoList { label :: String, list :: L.List String Todo } deriving (Show)

type MainUi = String

attrMap :: A.AttrMap
attrMap = A.attrMap V.defAttr
    [ (L.listSelectedAttr,    V.black `on` V.white)
    ]


handleEvent :: State -> T.BrickEvent () e -> T.EventM () (T.Next State)
handleEvent s (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt s
handleEvent s (T.VtyEvent e) = M.continue =<< T.handleEventLensed s todoList handleListEvent e
handleEvent s _ = M.continue s

drawUi :: State -> [Widget ()]
drawUi s = [todoView]
  where todoView  = createListView . view todoList $ s

app :: M.App State e ()
app = M.App { M.appDraw = drawUi
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = handleEvent
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
