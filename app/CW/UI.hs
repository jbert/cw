module CW.UI where

data Pt = Pt Double Double
    deriving (Show, Eq)

data Input = Quit | Mouse Pt
    deriving (Show, Eq)

type DrawLine = (Pt, Pt) -> IO ()

type Drawer = DrawLine -> IO ()

class Drawable a where
    draw :: a -> DrawLine -> IO ()
