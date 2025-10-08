module CW.UI.Screen where

import Data.Text (Text)
import Data.Word
import qualified SDL
import qualified SDL.Font as TTF

data Config = Config
    { width :: Int
    , height :: Int
    , name :: Text
    , bgColour :: SDL.V4 Word8
    , fgColour :: SDL.V4 Word8
    , font :: TTF.Font
    }
