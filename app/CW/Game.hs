module CW.Game where

import qualified CW.UI as UI

data State = State
    { ticks :: Integer
    , gold :: Integer
    , shouldQuit :: Bool
    , lastClick :: Maybe UI.Pt
    }
    deriving (Show)

initialState :: State
initialState = State 0 0 False Nothing
