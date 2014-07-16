{-# LANGUAGE TemplateHaskell #-}

module Engine.WorldState where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW

import Engine.Entity
import Engine.Input
import Util.Vector

data WorldState = WorldState
    { _dT :: GLfloat
    , _timeElapsed :: GLfloat
    , _inputState :: InputState

    , _player :: Entity
    }
makeLenses ''WorldState
newWorldState :: WorldState
newWorldState = WorldState 0 0 newInputState newEntity

frameRateLimit :: GLfloat
frameRateLimit = 60

inputDriver :: IORef WorldState -> IO (Maybe (IO WorldState))
inputDriver worldState = do
    dt <- realToFrac <$> get GLFW.time
    GLFW.time $= 0

    threadDelay $ floor $ 1000000 * (1 - dt) / frameRateLimit

    world <- readIORef worldState

    i <- getInput
    let w = world
            & inputState .~ i
            & dT .~ dt
    writeIORef worldState w

    let c = world ^. inputState . isClosed
    return (if c then Nothing else Just (return world))

worldUpdate :: IO WorldState -> IO WorldState -> IO WorldState
worldUpdate input state = do
    i <- input
    s <- state

    return $ s
        & timeElapsed +~ i ^. dT
        & player . pos . x .~ sin (s ^. timeElapsed)
