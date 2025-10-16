module CW.UI.KeyMap where

import CW.UI.Button (Button (..))
import qualified CW.UI.Button as Button
import CW.UI.Input (Input (..))

import qualified SDL

import Data.Map (Map)
import qualified Data.Map as Map

type KeyMap = Map SDL.Keycode Input

toSDL :: Char -> SDL.Keycode
toSDL 'q' = SDL.KeycodeQ
toSDL '1' = SDL.Keycode1
toSDL '2' = SDL.Keycode2
toSDL _ = error "unmapped key"

fromButtons :: [Button] -> KeyMap
fromButtons buttons = Map.mapKeys toSDL $ Map.union fixed $ Map.fromList $ map (\b -> (Button.shortcut b, Button.input b)) buttons

lookup :: KeyMap -> SDL.Keycode -> Maybe Input
lookup km kc = Map.lookup kc km

fixed :: Map Char Input
fixed = Map.fromList [('q', Quit)]
