module CW.UI.Pt where

data Pt = Pt Double Double
    deriving (Show, Eq, Ord)

mk :: Double -> Double -> Pt
mk = Pt

add :: Pt -> Pt -> Pt
add (Pt ax ay) (Pt bx by) = Pt (ax + bx) (ay + by)

neg :: Pt -> Pt
neg (Pt x y) = Pt (-x) (-y)

sub :: Pt -> Pt -> Pt
sub a b = add a (neg b)
