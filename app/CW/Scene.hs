module CW.Scene where

import qualified CW.Game as Game
import qualified CW.UI as UI
import qualified CW.UI.Circle as Circle
import CW.UI.Pt (Pt (..))
import CW.UI.Rect (Rect (..))
import CW.UI.Screen (Config (..))
import qualified CW.UI.Screen as Screen

data Scene = Scene {ticks :: Integer, drawables :: [UI.Drawer], confDrawables :: [UI.ConfigDrawer]}

mk :: Game.State -> Maybe Scene
mk gs
    | Game.shouldQuit gs = Nothing
    | otherwise = Just $ Scene ts ds [mkUI]
  where
    ts = Game.ticks gs
    ds = mkDrawers (Game.lastClick gs)

-- cds = mkUI

mkDrawers :: Maybe Pt -> [UI.Drawer]
mkDrawers Nothing = []
mkDrawers (Just p) = [Circle.drawCircle c]
  where
    r = 0.1
    c = Circle.mk p r

-- Right hand edge in Game coords (max x)
rh :: Config -> Double
rh conf = fromIntegral w / fromIntegral h
  where
    w = Screen.width conf
    h = Screen.height conf

mkUI :: UI.ConfigDrawer
mkUI conf dl = do
    dl (Pt uil uit, Pt uil uib)
    UI.drawRect (butRect 0) dl
    UI.drawRect (butRect 1) dl
    UI.drawRect (butRect 2) dl
  where
    uit = 0.0
    uil = 1.0
    uib = 1.0
    uir = rh conf
    eps = 0.01
    l = uil + eps
    r = uir - eps
    t = uit + eps
    -- b = uib - eps
    butH = 0.1 - (2 * eps)
    butT :: Int -> Double
    butT n = t + butH * fromIntegral n
    butB :: Int -> Double
    butB n = butT n + butH
    butRect n = Rect (Pt l $ butB n) (Pt r $ butT n)
