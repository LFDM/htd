module ConfirmDelete (createConfirmDeleteView) where

import Register
import Style

import Brick.Types
  ( Widget
  )

import Brick.Widgets.Core
  ( str
  , withAttr
  )

confirmText = "Are you sure you want to delete this todo? Hit enter to confirm or any other key to abort."

createConfirmDeleteView :: Widget Name
createConfirmDeleteView  = withAttr preTextAttr $ str confirmText

