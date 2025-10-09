module CW.UI.Rect where

import CW.UI.Pt (Pt (..))
import qualified CW.UI.Pt as Pt

data Rect = Rect Pt Pt
    deriving (Show, Eq, Ord)

mk :: Pt -> Pt -> Rect
mk = Rect

add :: Rect -> Pt -> Rect
add (Rect bl tr) p = Rect (Pt.add bl p) (Pt.add tr p)

contains :: Rect -> Pt -> Bool
contains (Rect (Pt l b) (Pt r t)) (Pt x y) =
    inside
  where
    inside =
        and
            [ l <= x
            , x <= r
            , b <= y
            , y <= t
            ]
