{-# LANGUAGE OverloadedStrings #-}

module CW.SDL where

import Debug.Trace

import Control.Concurrent
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Foreign.C.Types
import GHC.Int
import qualified SDL
import qualified SDL.Font as TTF

import CW.Scene (Scene)
import qualified CW.Scene as Scene
import CW.UI.Button (Button (..))
import CW.UI.Input (Input (..))
import CW.UI.Pt (Pt (..))
import CW.UI.Rect (Rect (..))
import CW.UI.RegionMap (RegionMap)
import qualified CW.UI.RegionMap as RegionMap
import CW.UI.Screen (Config (..))

-- import qualified CW.UI.Screen as Screen

toSDLV2 :: Config -> Pt -> SDL.V2 CInt
toSDLV2 conf (Pt px py) = SDL.V2 (fromIntegral rx) (fromIntegral ry)
  where
    h = height conf
    x = px * fromIntegral h
    y = py * fromIntegral h
    rx = round x :: Integer
    ry = round y :: Integer

toSDL :: Config -> Pt -> SDL.Point SDL.V2 CInt
toSDL conf p = SDL.P $ toSDLV2 conf p

fromSDL :: Config -> SDL.Point SDL.V2 GHC.Int.Int32 -> Pt
-- fromSDL conf (SDL.P (SDL.V2 x y)) = trace ("SDL click: " ++ (show x) ++ "," ++ (show y)) (Pt px py)
fromSDL conf (SDL.P (SDL.V2 x y)) = Pt px py
  where
    h = height conf
    px = fromIntegral x / fromIntegral h
    py = fromIntegral y / fromIntegral h

eventToInput :: Config -> SDL.EventPayload -> Maybe Input
eventToInput _ (SDL.KeyboardEvent ke) =
    if SDL.keyboardEventKeyMotion ke == SDL.Pressed
        && SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == SDL.KeycodeQ
        then Just Quit
        else Nothing
eventToInput conf (SDL.MouseButtonEvent (SDL.MouseButtonEventData (Just _) SDL.Pressed _ _ _ pos)) = Just $ Mouse $ fromSDL conf pos
eventToInput _ _ = Nothing

sdlLoop :: Config -> SDL.Renderer -> Chan (Maybe Scene) -> Chan [Input] -> IO ()
sdlLoop conf renderer sceneChan inputChan = do
    events <- SDL.pollEvents
    let inputs = Maybe.mapMaybe (eventToInput conf . SDL.eventPayload) events

    msc <- readChan sceneChan
    case msc of
        Just sc -> do
            buttons <- drawScene conf renderer sc
            let rm = RegionMap.fromButtons buttons
            -- traceM ("RM: " ++ show rm)
            -- traceM ("IN: " ++ show inputs)
            let inputs' = findButtons rm inputs
            -- traceM ("IN': " ++ show inputs')
            writeChan inputChan inputs'
            sdlLoop conf renderer sceneChan inputChan
        Nothing -> return ()

findButtons :: RegionMap -> [Input] -> [Input]
findButtons rm (i@(Mouse p) : rest) =
    (if null buttonInputs then [i] else buttonInputs) ++ findButtons rm rest
  where
    buttonInputs = RegionMap.find rm p
findButtons rm (i : rest) =
    (i : findButtons rm rest)
findButtons _ [] = []

drawLine :: Config -> SDL.Renderer -> (Pt, Pt) -> IO ()
drawLine conf renderer (p1, p2) = do
    SDL.drawLine renderer (toSDL conf p1) (toSDL conf p2)

drawRect :: Config -> SDL.Renderer -> Rect -> IO ()
drawRect conf renderer (Rect (Pt l b) (Pt r t)) = do
    SDL.drawLine renderer (pt l b) (pt r b)
    SDL.drawLine renderer (pt r b) (pt r t)
    SDL.drawLine renderer (pt r t) (pt l t)
    SDL.drawLine renderer (pt l t) (pt l b)
  where
    pt x y = toSDL conf (Pt x y)

drawScene :: Config -> SDL.Renderer -> Scene -> IO [Button]
drawScene conf renderer scene = do
    SDL.rendererDrawColor renderer SDL.$= bgColour conf
    SDL.clear renderer
    SDL.rendererDrawColor renderer SDL.$= fgColour conf

    let ls = [(Pt 0.1 0.1, Pt 0.8 0.8)]
    mapM_ (drawLine conf renderer) ls
    mapM_ (\d -> d (drawLine conf renderer)) (Scene.drawables scene)
    buttons' <- mapM (\dc -> dc conf (drawLine conf renderer)) (Scene.confDrawables scene)
    let buttons = concat buttons'

    surface <- TTF.solid (font conf) (fgColour conf) $ Text.pack $ show (Scene.ticks scene)
    texture <- SDL.createTextureFromSurface renderer surface
    let bl = toSDL conf (Pt 0.1 0.1)
    let tr = toSDLV2 conf (Pt 0.4 0.1)
    let dstRect = SDL.Rectangle bl tr
    SDL.copy renderer texture Nothing (Just dstRect)
    SDL.freeSurface surface

    SDL.present renderer
    return buttons

sdlMain :: Chan (Maybe Scene) -> Chan [Input] -> IO ()
sdlMain sceneChan inputChan = do
    SDL.initializeAll
    TTF.initialize

    fnt <- TTF.load "DejaVuSans.ttf" 48

    let conf =
            Config
                { width = 1024
                , height = 768
                , name = "CW"
                , font = fnt
                , bgColour = SDL.V4 0 0 255 255
                , fgColour = SDL.V4 0 255 0 0
                }
    window <-
        SDL.createWindow
            (name conf)
            SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 (fromIntegral $ width conf) (fromIntegral $ height conf)}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    sdlLoop conf renderer sceneChan inputChan
    TTF.free fnt
    SDL.destroyWindow window
