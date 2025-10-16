module CW.UI.Button where

import CW.UI.Input (Input)
import CW.UI.Rect (Rect)

data Button = Button {rect :: Rect, shortcut :: Char, input :: Input}
