module CW.UI.Pt where

data Pt = Pt Double Double
    deriving (Show, Eq, Ord)

mk :: Double -> Double -> Pt
mk = Pt

add :: Pt -> Pt -> Pt
add (Pt ax ay) (Pt bx by) = Pt (ax + bx) (ay + by)
