module CW.UI.RegionMap where

import CW.UI.Button (Button (..))
import CW.UI.Input (Input (..))
import CW.UI.Pt (Pt (..))
import CW.UI.Rect (Rect)
import qualified CW.UI.Rect as Rect

import Data.Map (Map)
import qualified Data.Map as Map

type RegionMap = Map Rect Input

find :: RegionMap -> Pt -> [Input]
find rm p = Map.elems $ Map.filterWithKey (\r _ -> Rect.contains r p) rm

empty :: RegionMap
empty = Map.empty

fromButtons :: [Button] -> RegionMap
fromButtons buttons = Map.fromList $ map (\(Button r i) -> (r, i)) buttons
