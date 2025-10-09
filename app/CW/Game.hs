module CW.Game where

import CW.UI.Pt (Pt)

data Mode = Square | Circle
    deriving (Show, Eq)

data State = State
    { ticks :: Integer
    , shouldQuit :: Bool
    , lastClick :: Maybe Pt
    , mode :: Mode
    }
    deriving (Show)

initialState :: State
initialState = State 0 False Nothing Circle
