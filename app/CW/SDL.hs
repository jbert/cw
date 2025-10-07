{-# LANGUAGE OverloadedStrings #-}

module CW.SDL where

import Control.Concurrent
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import Foreign.C.Types
import qualified SDL
import qualified SDL.Font as TTF

import CW.Scene (Scene)
import qualified CW.Scene as Scene
import CW.UI

data Config = Config
    { width :: Int
    , height :: Int
    , name :: Text
    , bgColour :: SDL.V4 Word8
    , fgColour :: SDL.V4 Word8
    , font :: TTF.Font
    }

toSDLV2 :: Config -> Pt -> SDL.V2 CInt
toSDLV2 conf (Pt px py) = SDL.V2 (fromIntegral rx) (fromIntegral ry)
  where
    w = width conf
    h = height conf
    x = px * fromIntegral w
    y = py * fromIntegral h
    rx = round x :: Integer
    ry = round y :: Integer

toSDL :: Config -> Pt -> SDL.Point SDL.V2 CInt
toSDL conf p = SDL.P $ toSDLV2 conf p

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

eventToInput :: SDL.EventPayload -> Maybe Input
eventToInput (SDL.KeyboardEvent ke) =
    if SDL.keyboardEventKeyMotion ke == SDL.Pressed
        && SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == SDL.KeycodeQ
        then Just Quit
        else Nothing
eventToInput _ = Nothing

sdlLoop :: Config -> SDL.Renderer -> Chan (Maybe Scene) -> Chan [Input] -> IO ()
sdlLoop conf renderer sceneChan inputChan = do
    events <- SDL.pollEvents
    let inputs = Maybe.mapMaybe (eventToInput . SDL.eventPayload) events

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

drawScene :: Config -> SDL.Renderer -> Scene -> IO ()
drawScene conf renderer scene = do
    SDL.rendererDrawColor renderer SDL.$= bgColour conf
    SDL.clear renderer
    SDL.rendererDrawColor renderer SDL.$= fgColour conf

    let ls = [(Pt 0.1 0.1, Pt 0.8 0.8)]
    mapM_ (drawLine conf renderer) ls

    surface <- TTF.solid (font conf) (fgColour conf) $ Text.pack $ show (Scene.ticks scene)
    texture <- SDL.createTextureFromSurface renderer surface
    let bl = toSDL conf (Pt 0.1 0.1)
    let tr = toSDLV2 conf (Pt 0.4 0.1)
    let dstRect = SDL.Rectangle bl tr
    SDL.copy renderer texture Nothing (Just dstRect)
    SDL.freeSurface surface

    SDL.present renderer
