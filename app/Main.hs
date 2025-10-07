module Main where

import Debug.Trace

import Control.Concurrent
import Control.Monad.State

import qualified CW.Game as Game
import qualified CW.SDL as SDL
import CW.Scene (Scene)
import qualified CW.Scene as Scene
import qualified CW.UI as UI

tick :: StateT Game.State IO ()
tick = do
    gs <- get
    put gs{Game.ticks = succ $ Game.ticks gs}

handleInput :: UI.Input -> StateT Game.State IO ()
handleInput UI.Quit = do
    modify (\s -> s{Game.shouldQuit = True})
handleInput (UI.Mouse p) = do
    traceM ("Mouse click: " ++ show p)
    modify (\s -> s{Game.lastClick = Just p})

handleInputs :: Chan [UI.Input] -> StateT Game.State IO ()
handleInputs inputChan = do
    inputs <- liftIO $ readChan inputChan
    mapM_ handleInput inputs

gameLoop :: Chan (Maybe Scene) -> Chan [UI.Input] -> StateT Game.State IO ()
gameLoop sceneChan inputChan = do
    tick
    gs <- get
    let scene = Scene.mk gs
    liftIO $ writeChan sceneChan scene
    handleInputs inputChan
    liftIO $ threadDelay 1000
    -- We rely on sending 'Nothing' on sceneChan for exit
    gameLoop sceneChan inputChan

gameMain :: Chan (Maybe Scene) -> Chan [UI.Input] -> IO ()
gameMain sceneChan inputChan = do
    ((), gs) <- runStateT (gameLoop sceneChan inputChan) Game.initialState
    print gs
    return ()

main :: IO ()
main = do
    sceneChan <- newChan
    inputChan <- newChan
    _ <- forkIO $ gameMain sceneChan inputChan
    -- Have sdl be the primary thread so it can exit on keypress
    SDL.sdlMain sceneChan inputChan
