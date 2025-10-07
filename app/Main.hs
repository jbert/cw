module Main where

import Debug.Trace

import Control.Concurrent
import Control.Monad.State

import qualified CW.SDL as SDL
import CW.Scene (Scene)
import qualified CW.Scene as Scene
import qualified CW.UI as UI

data GameState = GameState
    { ticks :: Integer
    , gold :: Integer
    , shouldQuit :: Bool
    , lastClick :: Maybe UI.Pt
    }
    deriving (Show)

initialGameState :: GameState
initialGameState = GameState 0 0 False Nothing

tick :: StateT GameState IO ()
tick = do
    gs <- get
    put gs{ticks = succ $ ticks gs}

mkScene :: StateT GameState IO (Maybe Scene)
mkScene = do
    sq <- gets shouldQuit
    ts <- gets ticks
    return
        $ if sq
            then
                Nothing
            else
                Just $ Scene.mk ts

handleInput :: UI.Input -> StateT GameState IO ()
handleInput UI.Quit = do
    modify (\s -> s{shouldQuit = True})
handleInput (UI.Mouse p) = do
    modify (\s -> s{lastClick = Just p})

handleInputs :: Chan [UI.Input] -> StateT GameState IO ()
handleInputs inputChan = do
    inputs <- liftIO $ readChan inputChan
    traceM ("Inputs: " ++ show inputs)
    mapM_ handleInput inputs

gameLoop :: Chan (Maybe Scene) -> Chan [UI.Input] -> StateT GameState IO ()
gameLoop sceneChan inputChan = do
    tick
    scene <- mkScene
    liftIO $ writeChan sceneChan scene
    handleInputs inputChan
    liftIO $ threadDelay 1000
    -- We rely on sending 'Nothing' on sceneChan for exit
    gameLoop sceneChan inputChan

gameMain :: Chan (Maybe Scene) -> Chan [UI.Input] -> IO ()
gameMain sceneChan inputChan = do
    ((), gs) <- runStateT (gameLoop sceneChan inputChan) initialGameState
    print gs
    return ()

main :: IO ()
main = do
    sceneChan <- newChan
    inputChan <- newChan
    _ <- forkIO $ gameMain sceneChan inputChan
    -- Have sdl be the primary thread so it can exit on keypress
    SDL.sdlMain sceneChan inputChan
