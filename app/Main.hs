module Main where

import Control.Concurrent
import Control.Monad.State

import qualified CW.Game as Game
import qualified CW.SDL as SDL
import CW.Scene (Scene)
import qualified CW.Scene as Scene
import CW.UI.Input (Input (..))

tick :: StateT Game.State IO ()
tick = do
    gs <- get
    put gs{Game.ticks = succ $ Game.ticks gs}

handleInput :: Input -> StateT Game.State IO ()
handleInput Quit = do
    modify (\s -> s{Game.shouldQuit = True})
handleInput (Mouse p) = do
    modify (\s -> s{Game.lastClick = Just p})
handleInput ButtonCircle = do
    modify (\s -> s{Game.mode = Game.Circle})
handleInput ButtonSquare = do
    modify (\s -> s{Game.mode = Game.Square})

handleInputs :: Chan [Input] -> StateT Game.State IO ()
handleInputs inputChan = do
    inputs <- liftIO $ readChan inputChan
    mapM_ handleInput inputs

buttonInputs :: [(Char, Input)]
buttonInputs =
    [ ('1', ButtonCircle)
    , ('2', ButtonSquare)
    ]

gameLoop :: Chan (Maybe Scene) -> Chan [Input] -> StateT Game.State IO ()
gameLoop sceneChan inputChan = do
    tick
    gs <- get
    let scene = Scene.mk buttonInputs gs
    liftIO $ writeChan sceneChan scene
    handleInputs inputChan
    liftIO $ threadDelay (20 * 1000)
    -- We rely on sending 'Nothing' on sceneChan for exit
    gameLoop sceneChan inputChan

gameMain :: Chan (Maybe Scene) -> Chan [Input] -> IO ()
gameMain sceneChan inputChan = do
    gs <- execStateT (gameLoop sceneChan inputChan) Game.initialState
    print gs
    return ()

main :: IO ()
main = do
    sceneChan <- newChan
    inputChan <- newChan
    _ <- forkIO $ gameMain sceneChan inputChan
    -- Have sdl be the primary thread so it can exit on keypress
    SDL.sdlMain sceneChan inputChan
