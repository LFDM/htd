module Style where

import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import Brick.Util (on, fg)

titleAttr :: A.AttrName
titleAttr = A.attrName "titleAttr"

preTextAttr :: A.AttrName
preTextAttr = A.attrName "preTextAttr"

styleMap :: A.AttrMap
styleMap = A.attrMap V.defAttr
    [ (L.listSelectedAttr,    V.black `on` V.white)
    ,  (titleAttr,            V.defAttr `V.withStyle` V.bold)
    ,  (preTextAttr,          fg gray `V.withStyle` V.bold)
    ]


gray :: V.Color
gray = V.rgbColor 128 128 128

lightGray :: V.Color
lightGray = V.rgbColor 192 192 192


