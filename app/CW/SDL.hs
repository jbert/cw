{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CW.SDL where

import Control.Concurrent
import Control.Monad.Reader
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
import CW.UI.RegionMap (RegionMap)
import qualified CW.UI.RegionMap as RegionMap
import CW.UI.Screen (Config (..))

-- import qualified CW.UI.Screen as Screen

toSDLV2 :: (MonadReader Config m) => Pt -> m (SDL.V2 CInt)
toSDLV2 (Pt px py) = do
    h <- asks height
    let x = px * fromIntegral h
        y = py * fromIntegral h
        rx = round x :: Integer
        ry = round y :: Integer
    return $ SDL.V2 (fromIntegral rx) (fromIntegral ry)

toSDL :: (MonadReader Config m) => Pt -> m (SDL.Point SDL.V2 CInt)
toSDL p = do
    sp <- toSDLV2 p
    return $ SDL.P sp

fromSDL :: (MonadReader Config m) => SDL.Point SDL.V2 GHC.Int.Int32 -> m Pt
fromSDL (SDL.P (SDL.V2 x y)) = do
    h <- asks height
    let
        px = fromIntegral x / fromIntegral h
        py = fromIntegral y / fromIntegral h
    return $ Pt px py

eventToInput :: (MonadReader Config m) => SDL.EventPayload -> m (Maybe Input)
eventToInput (SDL.KeyboardEvent ke) =
    if SDL.keyboardEventKeyMotion ke == SDL.Pressed
        && SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == SDL.KeycodeQ
        then return $ Just Quit
        else return Nothing
eventToInput (SDL.MouseButtonEvent (SDL.MouseButtonEventData (Just _) SDL.Pressed _ _ _ pos)) = do
    p <- fromSDL pos
    return $ Just $ Mouse p
eventToInput _ = return Nothing

findButtons :: RegionMap -> [Input] -> [Input]
findButtons rm (i@(Mouse p) : rest) =
    (if null buttonInputs then [i] else buttonInputs) ++ findButtons rm rest
  where
    buttonInputs = RegionMap.find rm p
findButtons rm (i : rest) = i : findButtons rm rest
findButtons _ [] = []

drawLine :: Config -> SDL.Renderer -> (Pt, Pt) -> IO ()
drawLine conf renderer (p1, p2) = do
    sp1 <- runReaderT (toSDL p1) conf
    sp2 <- runReaderT (toSDL p2) conf
    SDL.drawLine renderer sp1 sp2

drawScene :: (MonadIO m, MonadReader Config m) => SDL.Renderer -> Scene -> m [Button]
drawScene renderer scene = do
    bg <- asks bgColour
    fg <- asks fgColour
    fnt <- asks font

    SDL.rendererDrawColor renderer SDL.$= bg
    SDL.clear renderer
    SDL.rendererDrawColor renderer SDL.$= fg

    let ls = [(Pt 0.1 0.1, Pt 0.8 0.8)]

    conf <- ask
    liftIO $ mapM_ (drawLine conf renderer) ls
    liftIO $ mapM_ (\d -> d (drawLine conf renderer)) $ Scene.drawables scene
    buttons' <- liftIO $ mapM (\dc -> dc conf (drawLine conf renderer)) (Scene.confDrawables scene)
    let buttons = concat buttons'

    surface <- TTF.solid fnt fg $ Text.pack $ show (Scene.ticks scene)
    texture <- SDL.createTextureFromSurface renderer surface
    bl <- toSDL (Pt 0.1 0.1)
    tr <- toSDLV2 (Pt 0.4 0.1)
    let dstRect = SDL.Rectangle bl tr
    SDL.copy renderer texture Nothing (Just dstRect)
    SDL.freeSurface surface

    SDL.present renderer
    return buttons

sdlLoop :: (MonadIO m, MonadReader Config m) => SDL.Renderer -> Chan (Maybe Scene) -> Chan [Input] -> m ()
sdlLoop renderer sceneChan inputChan = do
    events <- SDL.pollEvents
    minputs <- mapM (eventToInput . SDL.eventPayload) events
    let inputs = Maybe.catMaybes minputs

    msc <- liftIO $ readChan sceneChan
    case msc of
        Just sc -> do
            buttons <- drawScene renderer sc
            let rm = RegionMap.fromButtons buttons
            let inputs' = findButtons rm inputs
            liftIO $ writeChan inputChan inputs'
            sdlLoop renderer sceneChan inputChan
        Nothing -> return ()

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

    _ <- runReaderT (sdlLoop renderer sceneChan inputChan) conf
    TTF.free fnt
    SDL.destroyWindow window
