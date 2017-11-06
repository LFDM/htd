module Editor
( Editor
, emptyEditor
, createEditor
, createEditorView
, handleEditorEvent
, getEditorText
, moveToStart
, moveToEnd
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
import qualified Data.Text.Zipper as Z
import Register

type Editor = E.Editor String Name

maxRows = Just 1

createEditor :: String -> Editor
createEditor = E.editor TODO_FORM maxRows

emptyEditor :: Editor
emptyEditor = createEditor ""

createEditorView :: Editor -> Widget Name
createEditorView = E.renderEditor (str . unlines) True

handleEditorEvent :: V.Event -> Editor -> T.EventM Name Editor
handleEditorEvent = E.handleEditorEvent

getEditorText :: Editor -> String
getEditorText = unlines . E.getEditContents

moveToStart :: Editor -> Editor
moveToStart = move (0, 0)

moveToEnd :: Editor -> Editor
moveToEnd e= move (lastPos getLineLengths) e
  where getLineLengths = view editorContentL e
        lastPos [] = (0, 0)
        lastPos xs = (length xs - 1, last xs)

move :: (Int, Int) -> Editor -> Editor
move pos = E.applyEdit (Z.moveCursor pos)
