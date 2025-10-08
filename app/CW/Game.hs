module CW.Game where

import CW.UI.Pt (Pt)

data State = State
    { ticks :: Integer
    , shouldQuit :: Bool
    , lastClick :: Maybe Pt
    }
    deriving (Show)

initialState :: State
initialState = State 0 False Nothing
