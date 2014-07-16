{-# LANGUAGE TemplateHaskell #-}

module Engine.Input where

import Control.Applicative
import Control.Lens
import Graphics.UI.GLFW as GLFW

import Engine.Entity

data InputState = InputState
    { _isClosed :: Bool
    , _mouseClicked :: Bool
    }
makeLenses ''InputState
newInputState :: InputState
newInputState = InputState False False

getInput :: IO InputState
getInput = do
    m <- (== Press) <$> getMouseButton ButtonLeft
    c <- (== Press) <$> getKey ESC
    return $ InputState c m
