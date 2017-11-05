module Editor
( Editor
, emptyEditor
, createEditor
, createEditorView
, handleEditorEvent
) where

import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( str
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Register

type Editor = E.Editor String Name

createEditor :: String -> Editor
createEditor = E.editor TODO_FORM Nothing

emptyEditor :: Editor
emptyEditor = createEditor ""

createEditorView :: Editor -> Widget Name
createEditorView = E.renderEditor (str . unlines) True

handleEditorEvent :: V.Event -> Editor -> T.EventM Name Editor
handleEditorEvent = E.handleEditorEvent
