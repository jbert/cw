module CW.UI.Circle where

import qualified CW.UI as UI
import CW.UI.Pt (Pt (..))
import qualified CW.UI.Pt as Pt

data Circle = Circle {centre :: Pt, radius :: Double}

instance UI.Drawable Circle where
    draw = drawCircle

drawCircle :: Circle -> UI.DrawLine -> IO ()
drawCircle (Circle (Pt x y) r) dl = do
    let numSteps = 20.0
    let step = 2 * pi / numSteps
    let steps = [0, step .. 2 * pi]
    let xs = map ((+ x) . (* r) . cos) steps
    let ys = map ((+ y) . (* r) . sin) steps
    let pts = zipWith Pt xs ys
    mapM_ dl $ zip pts (tail pts)

mk :: Pt -> Double -> Circle
mk = Circle
