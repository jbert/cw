module CW.UI where

import CW.UI.Pt (Pt)
import qualified CW.UI.Pt as Pt

data Input = Quit | Mouse Pt
    deriving (Show, Eq)

type DrawLine = (Pt, Pt) -> IO ()

type Drawer = DrawLine -> IO ()

class Drawable a where
    draw :: a -> DrawLine -> IO ()
