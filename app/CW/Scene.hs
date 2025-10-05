module CW.Scene where

newtype Scene = Scene {ticks :: Integer}

mk :: Integer -> Scene
mk = Scene
