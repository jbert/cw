module CW.UI where

import CW.UI.Button (Button)
import CW.UI.Pt (Pt (..))
import CW.UI.Rect (Rect (..))
import CW.UI.Screen (Config)

type DrawLine = (Pt, Pt) -> IO ()

type Drawer = DrawLine -> IO ()

type ConfigDrawer = Config -> DrawLine -> IO [Button]

drawRect :: Rect -> DrawLine -> IO ()
drawRect (Rect (Pt l b) (Pt r t)) dl = do
    _ <- dl (Pt l b, Pt r b)
    _ <- dl (Pt r b, Pt r t)
    _ <- dl (Pt r t, Pt l t)
    _ <- dl (Pt l t, Pt l b)
    return ()
