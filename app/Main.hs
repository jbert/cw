module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Data.Maybe

import qualified CW.SDL as SDL
import CW.Scene (Scene)
import qualified CW.Scene as Scene

data GameState = GameState
    { ticks :: Integer
    , gold :: Integer
    }
    deriving (Show)

initialGameState :: GameState
initialGameState = GameState 0 0

tick :: StateT GameState IO ()
tick = do
    gs <- get
    put gs{ticks = succ $ ticks gs}

mkScene :: StateT GameState IO (Maybe Scene)
mkScene = do
    gs <- get
    let scene = Scene.mk (ticks gs)
    return $ Just scene

gameLoop :: Chan (Maybe Scene) -> StateT GameState IO ()
gameLoop sceneChan = do
    tick
    scene <- mkScene
    liftIO $ writeChan sceneChan scene
    liftIO $ threadDelay 1000
    unless
        (isNothing scene)
        (gameLoop sceneChan)

gameMain :: Chan (Maybe Scene) -> IO ()
gameMain sceneChan = do
    ((), gs) <- runStateT (gameLoop sceneChan) initialGameState
    print gs
    return ()

main :: IO ()
main = do
    sceneChan <- newChan
    _ <- forkIO $ gameMain sceneChan
    -- Have sdl be the primary thread so it can exit on keypress
    SDL.sdlMain sceneChan
