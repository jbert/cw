module CW.UI.Circle where

import qualified CW.UI as UI

data Circle = Circle {centre :: UI.Pt, radius :: Double}

instance UI.Drawable Circle where
    draw = drawCircle

drawCircle :: Circle -> UI.DrawLine -> IO ()
drawCircle (Circle (UI.Pt x y) r) dl = do
    let numSteps = 20
    let steps = [0 .. numSteps]
    let xs = map (+ x) $ map (* r) $ map cos $ map (* (2 * pi)) steps
    let ys = map (+ y) $ map (* r) $ map sin $ map (* (2 * pi)) steps
    let pts = zipWith UI.Pt xs ys
    mapM_ dl $ zip pts (tail pts)

mk :: UI.Pt -> Double -> Circle
mk = Circle
