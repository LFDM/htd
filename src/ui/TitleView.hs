module TitleView (createTitleView) where

import Control.Lens

import Brick.Types
  ( Widget
  )

import Brick.Widgets.Core
  ( (<+>)
  , str
  , vBox
  , withAttr
  )

import State
import Style
import Todos
import Todo

titleText = "htd - Haskell Todos"

createTitleView :: State -> Widget ()
createTitleView s = vBox [ t, p ]
  where t = withAttr titleAttr $ str titleText
        p = str $ view (currentTodos . path) s


