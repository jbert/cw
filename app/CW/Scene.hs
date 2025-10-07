module CW.Scene where

import qualified CW.Game as Game
import qualified CW.UI as UI
import qualified CW.UI.Circle as Circle

data Scene = Scene {ticks :: Integer, drawables :: [UI.Drawer]}

mk :: Game.State -> Maybe Scene
mk gs
    | Game.shouldQuit gs = Nothing
    | otherwise = Just $ Scene ts ds
  where
    ts = Game.ticks gs
    ds = mkDrawers (Game.lastClick gs)

mkDrawers :: Maybe UI.Pt -> [UI.Drawer]
mkDrawers Nothing = []
mkDrawers (Just p) = [UI.draw c]
  where
    r = 0.05
    c = Circle.mk p r
