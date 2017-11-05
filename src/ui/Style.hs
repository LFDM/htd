module Style where

import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import Brick.Util (on)

titleAttr :: A.AttrName
titleAttr = A.attrName "titleAttr"

styleMap :: A.AttrMap
styleMap = A.attrMap V.defAttr
    [ (L.listSelectedAttr,    V.black `on` V.white)
    ,  (titleAttr,            V.withStyle V.bold)
    ]



