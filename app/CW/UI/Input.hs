module CW.UI.Input where

import CW.UI.Pt (Pt)

data Input = Quit | Mouse Pt | ButtonCircle | ButtonSquare
    deriving (Show, Eq)
