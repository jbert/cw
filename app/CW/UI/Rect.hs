module CW.UI.Rect where

import CW.UI.Pt (Pt)
import qualified CW.UI.Pt as Pt

data Rect = Rect Pt Pt
    deriving (Show, Eq)

add :: Rect -> Pt -> Rect
add (Rect bl tr) p = Rect (Pt.add bl p) (Pt.add tr p)
