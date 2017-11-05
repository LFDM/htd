module Editor where

import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( str
  )
import qualified Brick.Widgets.Edit as E
import Register

type Editor = E.Editor String Name

emptyEditor :: Editor
emptyEditor = (E.editor TODO_FORM Nothing "")

createEditorView :: Editor -> Widget Name
createEditorView = E.renderEditor (str . unlines) True
