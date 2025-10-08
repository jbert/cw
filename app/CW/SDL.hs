{-# LANGUAGE OverloadedStrings #-}

module CW.SDL where

import Debug.Trace

import Control.Concurrent
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import Foreign.C.Types
import GHC.Int
import qualified SDL
import qualified SDL.Font as TTF

import CW.Scene (Scene)
import qualified CW.Scene as Scene
import qualified CW.UI as UI
import CW.UI.Pt (Pt (..))

-- import qualified CW.UI.Pt as Pt
import CW.UI.Rect (Rect (..))

-- import qualified CW.UI.Rect as Rect

data Config = Config
    { width :: Int
    , height :: Int
    , name :: Text
    , bgColour :: SDL.V4 Word8
    , fgColour :: SDL.V4 Word8
    , font :: TTF.Font
    }

-- Right hand edge in Game coords (max x)
rh :: Config -> Double
rh conf = fromIntegral w / fromIntegral h
  where
    w = width conf
    h = height conf

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

eventToInput :: Config -> SDL.EventPayload -> Maybe UI.Input
eventToInput _ (SDL.KeyboardEvent ke) =
    if SDL.keyboardEventKeyMotion ke == SDL.Pressed
        && SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == SDL.KeycodeQ
        then Just UI.Quit
        else Nothing
eventToInput conf (SDL.MouseButtonEvent (SDL.MouseButtonEventData (Just _) SDL.Pressed _ _ _ pos)) = Just $ UI.Mouse $ fromSDL conf pos
eventToInput _ _ = Nothing

sdlLoop :: Config -> SDL.Renderer -> Chan (Maybe Scene) -> Chan [UI.Input] -> IO ()
sdlLoop conf renderer sceneChan inputChan = do
    events <- SDL.pollEvents
    let inputs = Maybe.mapMaybe (eventToInput conf . SDL.eventPayload) events

    msc <- readChan sceneChan
    case msc of
        Just sc -> do
            drawScene conf renderer sc
            writeChan inputChan inputs
            sdlLoop conf renderer sceneChan inputChan
        Nothing -> return ()

drawLine :: Config -> SDL.Renderer -> (Pt, Pt) -> IO ()
drawLine conf renderer (p1, p2) = do
    SDL.drawLine renderer (toSDL conf p1) (toSDL conf p2)

drawRect :: Config -> SDL.Renderer -> Rect -> IO ()
drawRect conf renderer rect@(Rect (Pt l b) (Pt r t)) = do
    -- traceM ("DR: " ++ (show rect))
    SDL.drawLine renderer (pt l b) (pt r b)
    SDL.drawLine renderer (pt r b) (pt r t)
    SDL.drawLine renderer (pt r t) (pt l t)
    SDL.drawLine renderer (pt l t) (pt l b)
  where
    pt x y = toSDL conf (Pt x y)

drawScene :: Config -> SDL.Renderer -> Scene -> IO ()
drawScene conf renderer scene = do
    SDL.rendererDrawColor renderer SDL.$= bgColour conf
    SDL.clear renderer
    SDL.rendererDrawColor renderer SDL.$= fgColour conf

    drawUI conf renderer
    let ls = [(Pt 0.1 0.1, Pt 0.8 0.8)]
    mapM_ (drawLine conf renderer) ls
    mapM_ (\d -> d (drawLine conf renderer)) (Scene.drawables scene)

    surface <- TTF.solid (font conf) (fgColour conf) $ Text.pack $ show (Scene.ticks scene)
    texture <- SDL.createTextureFromSurface renderer surface
    let bl = toSDL conf (Pt 0.1 0.1)
    let tr = toSDLV2 conf (Pt 0.4 0.1)
    let dstRect = SDL.Rectangle bl tr
    SDL.copy renderer texture Nothing (Just dstRect)
    SDL.freeSurface surface

    SDL.present renderer

drawUI :: Config -> SDL.Renderer -> IO ()
drawUI conf renderer = do
    drawLine conf renderer (Pt uil uit, Pt uil uib)
    drawRect conf renderer $ butRect 0
    drawRect conf renderer $ butRect 1
    drawRect conf renderer $ butRect 2
  where
    uit = 0.0
    uil = 1.0
    uib = 1.0
    uir = rh conf
    eps = 0.01
    l = uil + eps
    r = uir - eps
    t = uit + eps
    -- b = uib - eps
    butH = 0.1 - (2 * eps)
    butT :: Int -> Double
    butT n = t + butH * fromIntegral n
    butB :: Int -> Double
    butB n = butT n + butH
    butRect n = Rect (Pt l $ butB n) (Pt r $ butT n)

sdlMain :: Chan (Maybe Scene) -> Chan [UI.Input] -> IO ()
sdlMain sceneChan inputChan = do
    SDL.initializeAll
    TTF.initialize

    fnt <- TTF.load "DejaVuSans.ttf" 48

    let conf =
            Config
                { width = 1024
                , height = 768
                , name = "CW"
                , bgColour = SDL.V4 0 0 255 255
                , fgColour = SDL.V4 0 255 0 0
                , font = fnt
                }

    window <-
        SDL.createWindow
            (name conf)
            SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 (fromIntegral $ width conf) (fromIntegral $ height conf)}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    sdlLoop conf renderer sceneChan inputChan
    TTF.free fnt
    SDL.destroyWindow window
